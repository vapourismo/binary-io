{-# LANGUAGE FlexibleInstances #-}

module Communication.Stream
  ( streamBytes
  , writeBytesAtomically
  )
where

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString as ByteString

import System.IO (Handle)

-- | Stream contents of the handle into a lazy byte string.
streamBytes
  :: Handle -- ^ Handle to read from
  -> IO ByteString.Lazy.ByteString
streamBytes =
  ByteString.Lazy.hGetContents

-- | Write contents of the given lazy byte string all at once.
writeBytesAtomically
  :: Handle -- ^ Handle to write to
  -> ByteString.Lazy.ByteString -- ^ Bytes to be written
  -> IO ()
writeBytesAtomically handle payload =
  ByteString.hPut handle (ByteString.Lazy.toStrict payload)
