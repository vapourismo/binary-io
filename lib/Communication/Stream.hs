{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Communication.Stream
  ( ReaderError (..)
  , Reader
  , newReader

  , Writer
  , newWriter

  , Channel
  , newChannel

  , CanGet (..)
  , read
  , CanPut (..)
  , write

  , streamBytes
  , writeBytesAtomically
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
data ReaderError = ReaderGetError
  { readerErrorBodyRemaining :: ByteString.ByteString
  , readerErrorBodyOffset :: Binary.Get.ByteOffset
  , readerErrorBodyInput :: ByteString.ByteString
  , readerErrorMessage :: String
  }
  deriving (Show, Exception.Exception)

-- | Reader that reads from the same position every time
newtype PositionalReader = PositionalReader
  { runPositionalReader :: forall a. Binary.Get.Get a -> IO (PositionalReader, a) }

-- | Create a new positional reader.
newPositionalReader
  :: Handle -- ^ Handle that will be read from
  -> IO PositionalReader
newPositionalReader handle = do
  stream <- streamBytes handle
  pure (continue stream)
  where
    continue stream = PositionalReader $ \getter ->
      case Binary.Get.runGetOrFail getter stream of
        Left (remainingBody, offset, errorMessage) ->
          Exception.throw ReaderGetError
            { readerErrorBodyRemaining = remainingBody
            , readerErrorBodyOffset = offset
            , readerErrorBodyInput = stream
            , readerErrorMessage = errorMessage
            }

        Right (tailStream, _, value) ->
          pure (continue tailStream, value)

-- | Normal reader - will advance the reading position on each successful read
newtype Reader = Reader
  { runReader :: forall a. Binary.Get a -> IO a }

-- | Create a new reader.
newReader
  :: Handle -- ^ Handle that will be read from
  -> IO Reader
newReader handle = do
  posReader <- newPositionalReader handle
  readerVar <- MVar.newMVar posReader
  pure $ Reader $ \getter ->
    MVar.modifyMVar readerVar $ \posReader ->
      runPositionalReader posReader getter

-- * Writer

-- | Normal writer
newtype Writer = Writer
  { runWriter :: Binary.Put -> IO () }

-- | Create a writer.
newWriter
  :: Handle -- ^ Handle that will be written to
  -> Writer
newWriter handle = Writer $ \putter ->
  writeBytesAtomically handle (Binary.Put.runPut putter)

-- * Channel

-- | Pair of 'Reader' and 'Writer'
data Channel = Channel
  { channelWriter :: Writer
  , channelReader :: Reader
  }

-- | Create a new channel.
newChannel
  :: Handle -- ^ Handle that will be read from and written to
  -> IO Channel
newChannel handle =
  Channel (newWriter handle) <$> newReader handle

-- * Classes

-- | @r@ can execute 'Binary.Get' operations in @m@
class CanGet r where
  runGet
    :: r -- ^ Reader / source
    -> Binary.Get a -- ^ Operation to execute
    -> IO a

instance CanGet Reader where
  runGet = runReader

instance CanGet Channel where
  runGet = runGet . channelReader

-- | @w@ can execute 'Binary.Put' operations in @m@
class CanPut w where
  runPut
    :: w -- ^ Writer / target
    -> Binary.Put -- ^ Operation to execute
    -> IO ()

instance CanPut Writer where
  runPut = runWriter

instance CanPut Channel where
  runPut = runPut . channelWriter

-- | Read something from @r@.
read
  :: (CanGet r, Binary.Binary a)
  => r -- ^ Read source
  -> IO a
read reader =
  runGet reader Binary.get

-- | Write something to @w@.
write
  :: (CanPut w, Binary.Binary a)
  => w -- ^ Write target
  -> a -- ^ Value to be written
  -> IO ()
write writer value =
  runPut writer (Binary.put value)

-- * Utilities

-- | Stream contents of the handle into a lazy byte string.
streamBytes
  :: Handle -- ^ Handle to read from
  -> IO ByteString.ByteString
streamBytes =
  ByteString.hGetContents

-- | Write contents of the given lazy byte string all at once.
writeBytesAtomically
  :: Handle -- ^ Handle to write to
  -> ByteString.ByteString -- ^ Bytes to be written
  -> IO ()
writeBytesAtomically handle payload =
  ByteString.Strict.hPut handle (ByteString.toStrict payload)
