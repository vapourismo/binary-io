{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Communication.Channel
  ( ReaderError (..)
  , Reader
  , newReader
  , Writer
  , newWriter
  , Channel
  , newChannel
  , newPipe
  , CanGet (..)
  , read
  , CanPut (..)
  , write
  )
where

import Prelude hiding (read)

import qualified Control.Monad.Catch as Catch
import qualified Control.Concurrent.Classy.MVar as MVar
import qualified Control.Monad.Conc.Class as Conc

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.Binary as Binary

import System.IO (Handle)

import qualified Communication.Stream as Stream
import qualified Communication.Process as Process

-- | An error that can occur during reading
data ReaderError = ReaderGetError
  { readerErrorBodyRemaining :: ByteString.ByteString
  , readerErrorBodyOffset :: Binary.Get.ByteOffset
  , readerErrorBodyInput :: ByteString.ByteString
  , readerErrorMessage :: String
  }
  deriving (Show, Catch.Exception)

-- | Reader that reads from the same position every time
newtype PositionalReader m = PositionalReader
  { runPositionalReader :: forall a. Binary.Get.Get a -> m (PositionalReader m, a) }

-- | Create a new positional reader.
newPositionalReader
  :: (Catch.MonadThrow m, Stream.MonadByteStream m)
  => Handle -- ^ Handle that will be read from
  -> m (PositionalReader m)
newPositionalReader handle = do
  stream <- Stream.streamBytes handle
  pure (continue stream)
  where
    continue stream = PositionalReader $ \getter ->
      case Binary.Get.runGetOrFail getter stream of
        Left (remainingBody, offset, errorMessage) ->
          Catch.throwM ReaderGetError
            { readerErrorBodyRemaining = remainingBody
            , readerErrorBodyOffset = offset
            , readerErrorBodyInput = stream
            , readerErrorMessage = errorMessage
            }

        Right (tailStream, _, value) ->
          pure (continue tailStream, value)

-- | Normal reader - will advance the reading position on each successful read
newtype Reader m = Reader
  { runReader :: forall a. Binary.Get a -> m a }

-- | Create a new reader.
newReader
  :: (Catch.MonadThrow m, Stream.MonadByteStream m, Conc.MonadConc m)
  => Handle -- ^ Handle that will be read from
  -> m (Reader m)
newReader handle = do
  posReader <- newPositionalReader handle
  readerVar <- MVar.newMVar posReader
  pure $ Reader $ \getter ->
    MVar.modifyMVar readerVar $ \posReader ->
      runPositionalReader posReader getter

-- | Normal writer
newtype Writer m = Writer
  { runWriter :: Binary.Put -> m () }

-- | Create a writer.
newWriter
  :: Stream.MonadByteStream m
  => Handle -- ^ Handle that will be written to
  -> Writer m
newWriter handle = Writer $ \putter ->
  Stream.writeBytesAtomically handle (Binary.Put.runPut putter)

-- | Pair of 'Reader' and 'Writer'
data Channel m = Channel
  { channelWriter :: Writer m
  , channelReader :: Reader m
  }

-- | Create a new channel.
newChannel
  :: (Catch.MonadThrow m, Stream.MonadByteStream m, Conc.MonadConc m)
  => Handle -- ^ Handle that will be read from and written to
  -> m (Channel m)
newChannel handle =
  Channel (newWriter handle) <$> newReader handle

-- | Create a new pipe. It can be used to for interprocess communication.
newPipe
  :: ( Catch.MonadThrow m
     , Stream.MonadByteStream m
     , Conc.MonadConc m
     , Process.MonadProcess m
     )
  => m (Channel m)
newPipe = do
  (readHandle, writeHandle) <- Process.createPipe
  reader <- newReader readHandle
  pure Channel
    { channelWriter = newWriter writeHandle
    , channelReader = reader
    }

-- | @r@ can execute 'Binary.Get' operations
class CanGet r where
  runGet
    :: r m -- ^ Reader / source
    -> Binary.Get a -- ^ Operation to execute
    -> m a

instance CanGet Reader where
  runGet = runReader

instance CanGet Channel where
  runGet = runGet . channelReader

-- | @w@ can execute 'Binary.Put' operations
class CanPut w where
  runPut
    :: w m -- ^ Writer / target
    -> Binary.Put -- ^ Operation to execute
    -> m ()

instance CanPut Writer where
  runPut = runWriter

instance CanPut Channel where
  runPut = runPut . channelWriter

-- | Read something from @r@.
read
  :: (CanGet r, Binary.Binary a)
  => r m -- ^ Read source
  -> m a
read reader =
  runGet reader Binary.get

-- | Write something to @w@.
write
  :: (CanPut w, Binary.Binary a)
  => w m -- ^ Write target
  -> a -- ^ Value to be written
  -> m ()
write writer value =
  runPut writer (Binary.put value)
