{-# LANGUAGE DeriveAnyClass #-}

-- | Read and write values of types that implement 'Binary.Binary' from and to 'Handle's
module Data.Binary.IO
  ( -- * Readers
    ReaderError (..)

  , Reader
  , newReader

    -- * Writers
  , Writer
  , newWriter

    -- * Duplex
  , Duplex
  , newDuplex

    -- * Classes
  , CanGet (..)
  , read

  , CanPut (..)
  , write
  )
where

import Prelude hiding (read)

import qualified Control.Exception as Exception
import           Control.Monad (join)
import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.ByteString as ByteString.Strict
import qualified Data.ByteString.Lazy as ByteString
import           Data.IORef (IORef, atomicModifyIORef, newIORef)
import           System.IO (Handle, hSetBinaryMode)

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

-- | @since 0.0.1
newtype Reader = Reader (IORef StationaryReader)

runReader :: Reader -> Binary.Get a -> IO a
runReader (Reader readerVar) getter =
  -- We use 'atomicModifyIORef' which does not force the 'StationaryReader' to WHNF. Forcing the
  -- 'StationaryReader' might block indefinitely because it will try to read more from the
  -- underlying 'Handle'.
  join $ atomicModifyIORef readerVar $ \posReader ->
    case runStationaryReader posReader getter of
      Left error   -> (posReader, Exception.throwIO error)
      Right result -> pure <$> result

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
  Reader <$> newIORef posReader

-- * Writer

-- | @since 0.0.1
newtype Writer = Writer Handle

runWriter :: Writer -> Binary.Put -> IO ()
runWriter (Writer handle) putter =
  writeBytesAtomically handle (Binary.Put.runPut putter)

-- | Create a writer.
--
-- Other threads writing to the same 'Handle' do not interfere with the resulting 'Writer'. The
-- 'Writer' may be used concurrently.
--
-- @since 0.0.1
newWriter
  :: Handle -- ^ Handle that will be written to
  -> Writer
newWriter = Writer

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

-- | @w@ can execute 'Binary.Put' operations
--
-- @since 0.0.1
class CanPut w where
  runPut
    :: w -- ^ Writer / target
    -> Binary.Put -- ^ Operation to execute
    -> IO ()

instance CanPut Handle where
  runPut handle putter = writeBytesAtomically handle (Binary.Put.runPut putter)

instance CanPut Writer where
  runPut = runWriter

instance CanPut Duplex where
  runPut = runPut . duplexWriter

-- | Read something from @r@.
--
-- @since 0.0.1
read
  :: (CanGet r, Binary.Binary a)
  => r -- ^ Read source
  -> IO a
read reader =
  runGet reader Binary.get

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

-- | Write contents of the given lazy byte string all at once.
writeBytesAtomically
  :: Handle -- ^ Handle to write to
  -> ByteString.ByteString -- ^ Bytes to be written
  -> IO ()
writeBytesAtomically handle payload =
  ByteString.Strict.hPut handle (ByteString.toStrict payload)
