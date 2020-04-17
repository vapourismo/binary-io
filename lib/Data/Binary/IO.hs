{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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

  , CanGet (..)
  , read
  , CanPut (..)
  , write
  )
where

import Prelude hiding (read)

import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString as ByteString.Strict
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.Binary as Binary

import System.IO (Handle)

-- * Reader

-- | An error that can occur during reading
--
-- @since 1.0.0
data ReaderError = ReaderGetError -- ^ Error from the 'Binary.Get' operation
  { readerErrorRemaining :: !ByteString.ByteString
  -- ^ Unconsumed part of the byte stream
  --
  -- @since 1.0.0

  , readerErrorOffset :: !Binary.Get.ByteOffset
  -- ^ Error location represented as an offset into the input
  --
  -- @since 1.0.0

  , readerErrorInput :: !ByteString.ByteString
  -- ^ Input to the 'Binary.Get' operation
  --
  -- @since 1.0.0

  , readerErrorMessage :: !String
  -- ^ Error message
  --
  -- @since 1.0.0
  }
  deriving (Show, Exception.Exception)

newtype StationaryReader = StationaryReader
  { runStationaryReader :: forall a. Binary.Get.Get a -> IO (StationaryReader, a) }

-- | Create a new stationary reader that will read from the same position in the stream every time.
-- However, it will return a new StationaryReader which can be used to consume the rest of the
-- stream.
--
-- Reading using the resulting StationaryReader may throw a 'ReaderError'.
--
-- Other threads reading from the same 'Handle' will interfere with read operations of the
-- StationaryReader.
newStationaryReader
  :: Handle -- ^ Handle that will be read from
  -> IO StationaryReader
newStationaryReader handle = do
  stream <- ByteString.hGetContents handle
  pure (continue stream)
  where
    continue stream = StationaryReader $ \getter -> do
      -- Evaluate the result of 'runGetOrFail' to WHNF. This should be enough because it means that
      -- the parser has decided between 'Left' and 'Right'.
      result <- Exception.evaluate (Binary.Get.runGetOrFail getter stream)
      case result of
        Left (remainingBody, offset, errorMessage) ->
          Exception.throw ReaderGetError
            { readerErrorRemaining = remainingBody
            , readerErrorOffset = offset
            , readerErrorInput = stream
            , readerErrorMessage = errorMessage
            }

        Right (tailStream, _, value) ->
          pure (continue tailStream, value)

-- | @since 1.0.0
newtype Reader = Reader
  { runReader :: forall a. Binary.Get a -> IO a }

-- | Create a new reader.
--
-- Reading using the 'Reader' may throw 'ReaderError'.
--
-- The internal position of the 'Reader' is not advanced when it throws an exception during reading.
-- This has the consequence that if you're trying to read the same faulty 'Binary.Get' operation
-- multiple times, you will always receive an exception.
--
-- Other threads reading from the 'Handle' will interfere with read operations of the 'Reader'.
-- However, the 'Reader' itself is thread-safe and can be utilized in concurrently.
--
-- @since 1.0.0
newReader
  :: Handle -- ^ Handle that will be read from
  -> IO Reader
newReader handle = do
  posReader <- newStationaryReader handle
  readerVar <- MVar.newMVar posReader
  pure $ Reader $ \getter ->
    MVar.modifyMVar readerVar $ \posReader ->
      runStationaryReader posReader getter

-- * Writer

-- | @since 1.0.0
newtype Writer = Writer
  { runWriter :: Binary.Put -> IO () }

-- | Create a writer.
--
-- Other threads writing to the same 'Handle' do not interfere with the resulting 'Writer'. The
-- 'Writer' may be used concurrently.
--
-- @since 1.0.0
newWriter
  :: Handle -- ^ Handle that will be written to
  -> Writer
newWriter handle = Writer $ \putter ->
  writeBytesAtomically handle (Binary.Put.runPut putter)

-- * Duplex

-- | Pair of 'Reader' and 'Writer'
--
-- @since 1.0.0
data Duplex = Duplex
  { duplexWriter :: !Writer
  , duplexReader :: !Reader
  }

-- | Create a new duplex. The 'Duplex' inherits all the properties of 'Reader' and 'Writer' when
-- created with 'newReader' and 'newWriter'.
--
-- @since 1.0.0
newDuplex
  :: Handle -- ^ Handle that will be read from and written to
  -> IO Duplex
newDuplex handle =
  Duplex (newWriter handle) <$> newReader handle

-- * Classes

-- | @r@ can execute 'Binary.Get' operations
--
-- @since 1.0.0
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
-- @since 1.0.0
class CanPut w where
  runPut
    :: w -- ^ Writer / target
    -> Binary.Put -- ^ Operation to execute
    -> IO ()

instance CanPut Writer where
  runPut = runWriter

instance CanPut Duplex where
  runPut = runPut . duplexWriter

-- | Read something from @r@.
--
-- @since 1.0.0
read
  :: (CanGet r, Binary.Binary a)
  => r -- ^ Read source
  -> IO a
read reader =
  runGet reader Binary.get

-- | Write something to @w@.
--
-- @since 1.0.0
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
