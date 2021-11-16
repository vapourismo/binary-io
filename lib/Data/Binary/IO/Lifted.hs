{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Read and write values of types that implement 'Binary.Binary'.
module Data.Binary.IO.Lifted
  ( -- * Reader
    ReaderError (..)

  , Reader (..)
  , newReader
  , newReaderWith
  , mapReader

    -- * Writer
  , Writer (..)
  , newWriter
  , newWriterWith
  , mapWriter

    -- * Pipe
  , newPipe

    -- * Duplex
  , Duplex (..)
  , newDuplex
  , newDuplexWith
  , mapDuplex

    -- * Classes
  , CanGet (..)
  , read
  , isEmpty

  , CanPut (..)
  , write
  )
where

import Prelude hiding (read)

import           Control.Arrow ((&&&))
import qualified Control.Concurrent.Classy as Concurrent
import           Control.Monad (join, unless)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Get
import           Data.Binary.IO.Internal.AwaitNotify (newAwaitNotify, runAwait, runNotify)
import qualified Data.Binary.Put as Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Lazy (toStrict)
import           Data.IORef (atomicModifyIORef', mkWeakIORef, newIORef)
import qualified Deque.Strict as Deque
import           System.IO (Handle, hSetBinaryMode)
import           System.Mem.Weak (deRefWeak)

-- * Reader

-- | An error that can occur during reading
--
-- @since 0.4.0
data ReaderError = ReaderGetError -- ^ Error from the 'Binary.Get' operation
  { readerErrorRemaining :: !ByteString
  -- ^ Unconsumed part of the byte stream
  --
  -- @since 0.4.0

  , readerErrorOffset :: !Get.ByteOffset
  -- ^ Error location represented as an offset into the input
  --
  -- @since 0.4.0

  , readerErrorInput :: !ByteString
  -- ^ Input to the 'Binary.Get' operation
  --
  -- @since 0.4.0

  , readerErrorMessage :: !String
  -- ^ Error message
  --
  -- @since 0.4.0
  }
  deriving stock Show
  deriving anyclass Catch.Exception

newtype StationaryReader m = StationaryReader
  { runStationaryReader
      :: forall a
      .  Binary.Get a
      -> ExceptT ReaderError m (StationaryReader m, a)
  }

newStationaryReaderWith
  :: forall m
  .  Concurrent.MonadConc m
  => m ByteString
  -> m (StationaryReader m)
newStationaryReaderWith getChunk = do
  inputRef <- Concurrent.newIORef ByteString.empty

  let
    make = StationaryReader $ \get -> do
      input <- lift $ Concurrent.readIORef inputRef
      loop $ Get.pushChunk (Get.runGetIncremental get) input

    loop :: Get.Decoder a -> ExceptT ReaderError m (StationaryReader m, a)
    loop = \case
      Get.Fail remainingBody offset errorMessage -> do
        input <- lift $ Concurrent.readIORef inputRef
        except $ Left ReaderGetError
          { readerErrorRemaining = remainingBody
          , readerErrorOffset = offset
          , readerErrorInput = input
          , readerErrorMessage = errorMessage
          }

      Get.Done remainingBody _ value -> Catch.mask_ $ do
        lift $ Concurrent.writeIORef inputRef remainingBody
        pure (make, value)

      Get.Partial continue -> do
        chunk <- lift $ Catch.mask $ \restore -> do
          chunk <- restore getChunk
          if ByteString.null chunk then
            pure Nothing
          else
            Concurrent.atomicModifyIORef' inputRef $ (<> chunk) &&& const (Just chunk)

        loop $ continue chunk

  pure make

-- | @since 0.4.0
newtype Reader m = Reader
  { runReader :: forall a. Binary.Get a -> m a }

-- | Transform the underlying functor.
--
-- @since 0.4.0
mapReader :: (forall a. m a -> n a) -> Reader m -> Reader n
mapReader f (Reader run) = Reader (f . run)

-- | Create a new 'Reader' using an action that provides the chunks.
--
-- The chunk producers indicates the end of the stream by returning an empty
-- 'ByteString.ByteString'.
--
-- Reading using the 'Reader' may throw 'ReaderError'.
--
-- The internal position of the 'Reader' is not advanced when it throws an exception during reading.
-- This has the consequence that if you're trying to read with the same faulty 'Binary.Get'
-- operation multiple times, you will always receive an exception.
--
-- The 'Reader' is safe to use concurrently.
--
-- @since 0.4.0
newReaderWith
  :: Concurrent.MonadConc m
  => m ByteString -- ^ Chunk provider
  -> m (Reader m)
newReaderWith getChunk = do
  posReader <- newStationaryReaderWith getChunk
  mvar <- Concurrent.newMVar posReader
  pure $ Reader $ \get ->
    Concurrent.modifyMVar mvar $ \posReader -> do
      result <- runExceptT $ runStationaryReader posReader get
      either Catch.throwM pure result

-- | Create a new reader.
--
-- Inherits properties from 'newReaderWith'.
--
-- Other threads reading from the 'Handle' will interfere with read operations of the 'Reader'.
-- However, the 'Reader' itself is thread-safe and can be utilized concurrently.
--
-- The given 'Handle' will be swiched to binary mode via 'hSetBinaryMode'.
--
-- @since 0.4.0
newReader
  :: (Concurrent.MonadConc m, MonadIO m)
  => Handle -- ^ Handle to read from
  -> m (Reader m)
newReader handle = do
  liftIO $ hSetBinaryMode handle True
  newReaderWith $ liftIO $ ByteString.hGetSome handle 4096

-- | @r@ can execute 'Binary.Get' operations in @m@
--
-- @since 0.4.0
class CanGet r m where
  runGet :: r -> Binary.Get a -> m a

instance CanGet (Reader m) m where
  runGet reader get = runReader reader get

instance CanGet (Duplex m) m where
  runGet = runGet . duplexReader

-- | Read something from @r@. Inherits properties from 'runGet'.
--
-- @since 0.4.0
read
  :: (CanGet r m, Binary.Binary a)
  => r -- ^ Source to read from
  -> m a
read source = runGet source Binary.get

-- | Check if there is no more input to consume. This function may block. All properties of 'runGet'
-- apply to this function as well.
--
-- @since 0.4.0
isEmpty
  :: CanGet r m
  => r -- ^ Source to check for stream depletion
  -> m Bool
isEmpty source = runGet source Get.isEmpty

-- * Writer

-- | @since 0.4.0
newtype Writer m = Writer
  { runWriter :: forall a. Put.PutM a -> m a }

-- | Transform the underlying functor.
--
-- @since 0.4.0
mapWriter :: (forall x. m x -> n x) -> Writer m -> Writer n
mapWriter f (Writer write) = Writer (f . write)

-- | Create a writer using a function that handles the output chunks.
--
-- @since 0.4.0
newWriterWith
  :: Functor m
  => (ByteString -> m ()) -- ^ Chunk writer
  -> Writer m
newWriterWith write = Writer $ \put -> do
  let (result, body) = Put.runPutM put
  -- We allocate one big ByteString chunk to preserve write atomicity.
  result <$ write (toStrict body)

-- | Create a writer.
--
-- Other threads writing to the same 'Handle' do not interfere with the resulting 'Writer'. The
-- 'Writer' may be used concurrently.
--
-- @since 0.4.0
newWriter
  :: MonadIO m
  => Handle -- ^ Write target
  -> Writer m
newWriter handle =
  newWriterWith (liftIO . ByteString.hPut handle)

-- | @w@ can execute 'Binary.Put' operations in @m@
--
-- @since 0.4.0
class CanPut w m where
  runPut :: w -> Put.PutM a -> m a

instance CanPut (Writer m) m where
  runPut writer put = runWriter writer put

instance CanPut (Duplex m) m where
  runPut = runPut . duplexWriter

instance MonadIO m => CanPut Handle m where
  runPut handle put = do
    let (result, body) = Put.runPutM put
    result <$ liftIO (ByteString.hPut handle (toStrict body))

-- | Write something to @w@.
--
-- @since 0.4.0
write
  :: (CanPut w m, Binary.Binary a)
  => w -- ^ Write target
  -> a -- ^ Value to write
  -> m ()
write sink value = runPut sink $ Binary.put value

-- * Pipe

-- | Create a connected pair of 'Reader' and 'Writer'.
--
-- The 'Reader' will automatically end the stream if the 'Writer' goes out of scope.
--
-- @since 0.4.0
newPipe :: (Concurrent.MonadConc m, MonadIO m) => m (Reader m, Writer m)
newPipe = do
  chan <- liftIO $ newIORef mempty
  weakChan <- liftIO $ mkWeakIORef chan $ pure ()
  (await, notify) <- liftIO newAwaitNotify

  let
    read = do
      mbChan <- deRefWeak weakChan
      case mbChan of
        Nothing -> pure ByteString.empty
        Just chan -> join $
          atomicModifyIORef' chan $ \queue ->
            case Deque.uncons queue of
              Just (elem, queue) -> (queue, pure elem)
              Nothing -> (queue, runAwait await >> read)

    write msg =
      unless (ByteString.null msg) $ do
        atomicModifyIORef' chan $ \queue ->
          (Deque.snoc msg queue, ())
        runNotify notify

  reader <- newReaderWith (liftIO read)
  let writer = newWriterWith (liftIO . write)

  pure (reader, writer)

-- * Duplex

-- | Pair of 'Reader' and 'Writer'
--
-- @since 0.4.0
data Duplex m = Duplex
  { duplexWriter :: Writer m
  , duplexReader :: Reader m
  }

-- | Transform the underlying functor.
--
-- @since 0.4.0
mapDuplex :: (forall a. m a -> n a) -> Duplex m -> Duplex n
mapDuplex f (Duplex w r) = Duplex (mapWriter f w) (mapReader f r)

-- | Create a new duplex. The 'Duplex' inherits all the properties of 'Reader' and 'Writer' when
-- created with 'newReader' and 'newWriter'.
--
-- @since 0.4.0
newDuplex
  :: (Concurrent.MonadConc m, MonadIO m)
   => Handle -- ^ Handle to read from and write to
   -> m (Duplex m)
newDuplex handle = Duplex (newWriter handle) <$> newReader handle

-- | Combines 'newReaderWith' and 'newWriterWith'.
--
-- @since 0.4.0
newDuplexWith
  :: Concurrent.MonadConc m
  => m ByteString -- ^ Input chunk producer for 'Reader'
  -> (ByteString -> m ()) -- ^ Chunk writer for 'Writer'
  -> m (Duplex m)
newDuplexWith getChunk writeChunk = Duplex (newWriterWith writeChunk) <$> newReaderWith getChunk
