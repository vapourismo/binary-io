{-# LANGUAGE DeriveAnyClass #-}

-- | Read and write values of types that implement 'Binary.Binary' from and to 'Handle's
module Data.Binary.IO
  ( -- * Readers
    ReaderError (..)

  , Reader
  , newReader
  , newReaderWith

    -- * Writers
  , Writer
  , newWriter
  , newWriterWith

    -- * Pipe
  , newPipe

    -- * Duplex
  , Duplex (..)
  , newDuplex
  , newDuplexWith

    -- * Classes
  , CanGet (..)
  , read
  , isEmpty

  , CanPut (..)
  , write
  )
where

import Prelude hiding (read)

import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import           Control.Monad (unless, void)
import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.ByteString as ByteString.Strict
import qualified Data.ByteString.Lazy as ByteString
import           Data.ByteString.Lazy.Internal (ByteString (Chunk, Empty))
import           System.IO (Handle, hSetBinaryMode)
import           System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Mem.Weak as Weak

-- * Reader

-- | An error that can occur during reading
--
-- @since 0.0.1
data ReaderError = ReaderGetError -- ^ Error from the 'Binary.Get' operation
  { readerErrorRemaining :: !ByteString.ByteString
  -- ^ Unconsumed part of the byte stream
  --
  -- @since 0.0.1

  , readerErrorOffset :: !Binary.Get.ByteOffset
  -- ^ Error location represented as an offset into the input
  --
  -- @since 0.0.1

  , readerErrorInput :: !ByteString.ByteString
  -- ^ Input to the 'Binary.Get' operation
  --
  -- @since 0.0.1

  , readerErrorMessage :: !String
  -- ^ Error message
  --
  -- @since 0.0.1
  }
  deriving (Show, Exception.Exception)

newtype StationaryReader = StationaryReader ByteString.ByteString

runStationaryReader
  :: StationaryReader
  -> Binary.Get.Get a
  -> Either ReaderError (StationaryReader, a)
runStationaryReader (StationaryReader stream) getter =
  bimap withError withSuccess (Binary.Get.runGetOrFail getter stream)
  where
    withError (remainingBody, offset, errorMessage) =
      ReaderGetError
        { readerErrorRemaining = remainingBody
        , readerErrorOffset = offset
        , readerErrorInput = stream
        , readerErrorMessage = errorMessage
        }

    withSuccess (tailStream, _, value) = (StationaryReader tailStream, value)

newStationaryReader :: Handle -> IO StationaryReader
newStationaryReader handle = do
  hSetBinaryMode handle True
  StationaryReader <$> ByteString.hGetContents handle

newStationaryReaderWith :: IO ByteString.Strict.ByteString -> IO StationaryReader
newStationaryReaderWith get =
  StationaryReader <$> mkStream get

-- | @since 0.0.1
newtype Reader = Reader (MVar StationaryReader)

runReader :: Reader -> Binary.Get a -> IO a
runReader (Reader readerVar) getter =
  modifyMVar readerVar $ \posReader ->
    either Exception.throwIO pure (runStationaryReader posReader getter)

-- | Create a new reader.
--
-- Reading using the 'Reader' may throw 'ReaderError'.
--
-- The internal position of the 'Reader' is not advanced when it throws an exception during reading.
-- This has the consequence that if you're trying to read with the same faulty 'Binary.Get'
-- operation multiple times, you will always receive an exception.
--
-- Other threads reading from the 'Handle' will interfere with read operations of the 'Reader'.
-- However, the 'Reader' itself is thread-safe and can be utilized concurrently.
--
-- Once the 'Handle' reaches EOF, it will be closed.
--
-- The given 'Handle' will be swiched to binary mode via 'hSetBinaryMode'.
--
-- @since 0.0.1
newReader
  :: Handle -- ^ Handle that will be read from
  -> IO Reader
newReader handle = do
  posReader <- newStationaryReader handle
  Reader <$> newMVar posReader

-- | This function works very similar to 'newReader' except no 'Handle' is involved.
--
-- @since 0.1.1
newReaderWith
  :: IO ByteString.Strict.ByteString -- ^ Chunk producer
  -> IO Reader
newReaderWith get = do
  posReader <- newStationaryReaderWith get
  Reader <$> newMVar posReader

-- * Writer

-- | @since 0.0.1
newtype Writer = Writer (ByteString.Strict.ByteString -> IO ())

runWriter :: Writer -> Binary.Put -> IO ()
runWriter (Writer write) putter =
  write (ByteString.toStrict (Binary.Put.runPut putter))

-- | Create a writer.
--
-- Other threads writing to the same 'Handle' do not interfere with the resulting 'Writer'. The
-- 'Writer' may be used concurrently.
--
-- @since 0.0.1
newWriter
  :: Handle -- ^ Handle that will be written to
  -> Writer
newWriter handle =
  Writer (ByteString.Strict.hPut handle)

-- | Create a writer using a function that handles the output chunks.
--
-- @since 0.1.1
newWriterWith
  :: (ByteString.Strict.ByteString -> IO ()) -- ^ Chunk handler
  -> Writer
newWriterWith =
  Writer

-- * Pipe

-- | Create a connected pair of 'Reader' and 'Writer'.
--
-- @since 0.2.0
newPipe :: IO (Reader, Writer)
newPipe = do
  chan <- Chan.newChan
  mvar <- MVar.newMVar chan

  Weak.addFinalizer chan (void (MVar.tryTakeMVar mvar))

  let
    read = do
      mbChan <- MVar.tryReadMVar mvar
      maybe (pure ByteString.Strict.empty) Chan.readChan mbChan

    write msg =
      unless (ByteString.Strict.null msg) $
        Chan.writeChan chan msg

  reader <- newReaderWith read
  let writer = newWriterWith write

  pure (reader, writer)

-- * Duplex

-- | Pair of 'Reader' and 'Writer'
--
-- @since 0.0.1
data Duplex = Duplex
  { duplexWriter :: !Writer
  , duplexReader :: !Reader
  }

-- | Create a new duplex. The 'Duplex' inherits all the properties of 'Reader' and 'Writer' when
-- created with 'newReader' and 'newWriter'.
--
-- @since 0.0.1
newDuplex
  :: Handle -- ^ Handle that will be read from and written to
  -> IO Duplex
newDuplex handle =
  Duplex (newWriter handle) <$> newReader handle

-- | Combines 'newReaderWith' and 'newWriterWith'.
--
-- @since 0.1.1
newDuplexWith
  :: IO ByteString.Strict.ByteString
  -> (ByteString.Strict.ByteString -> IO ())
  -> IO Duplex
newDuplexWith get push =
  Duplex (newWriterWith push) <$> newReaderWith get

-- * Classes

-- | @r@ can execute 'Binary.Get' operations
--
-- @since 0.0.1
class CanGet r where
  runGet
    :: r -- ^ Reader / source
    -> Binary.Get a -- ^ Operation to execute
    -> IO a

instance CanGet Reader where
  runGet = runReader

instance CanGet Duplex where
  runGet = runGet . duplexReader

-- | Read something from @r@.
--
-- @since 0.0.1
read
  :: (CanGet r, Binary.Binary a)
  => r -- ^ Read source
  -> IO a
read reader =
  runGet reader Binary.get

-- | Check if there is no more input to consume. This function may block. All properties of 'runGet'
-- apply to this function as well.
--
-- @since 0.2.1
isEmpty :: CanGet r => r -> IO Bool
isEmpty reader = runGet reader Binary.Get.isEmpty

-- | @w@ can execute 'Binary.Put' operations
--
-- @since 0.0.1
class CanPut w where
  runPut
    :: w -- ^ Writer / target
    -> Binary.Put -- ^ Operation to execute
    -> IO ()

instance CanPut Handle where
  runPut handle putter =
    ByteString.Strict.hPut handle (ByteString.toStrict (Binary.Put.runPut putter))

instance CanPut Writer where
  runPut = runWriter

instance CanPut Duplex where
  runPut = runPut . duplexWriter

-- | Write something to @w@.
--
-- @since 0.0.1
write
  :: (CanPut w, Binary.Binary a)
  => w -- ^ Write target
  -> a -- ^ Value to be written
  -> IO ()
write writer value =
  runPut writer (Binary.put value)

-- * Utilities

-- | Construct a lazy 'ByteString.ByteString' from a function that retrieves chunks.
-- Returning an empty chunk indicates the end of the stream.
mkStream :: IO ByteString.Strict.ByteString -> IO ByteString.ByteString
mkStream get =
  readLazily
  where
    read = do
      chunk <- get
      if ByteString.Strict.null chunk then
        pure Empty
      else
        Chunk chunk <$> readLazily

    readLazily = unsafeInterleaveIO read
