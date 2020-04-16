{-# LANGUAGE FlexibleInstances #-}

module Communication.Stream 
  ( MonadByteStream (..)
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString as ByteString

import System.IO (Handle)

class MonadByteStream m where
  -- | Read everything from the handle as a lazy stream of bytes.
  streamBytes :: Handle -> m ByteString.Lazy.ByteString

  -- | Write all the bytes at once.
  writeBytesAtomically :: Handle -> ByteString.Lazy.ByteString -> m ()

instance MonadByteStream IO where
  streamBytes = ByteString.Lazy.hGetContents

  writeBytesAtomically handle payload = ByteString.hPut handle (ByteString.Lazy.toStrict payload)

-- | Automatically lifts 'MonadByteStream' through monad transformers
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadByteStream m, Monad m) => MonadByteStream (t m) where
  streamBytes handle = lift (streamBytes handle)

  writeBytesAtomically handle payload = lift (writeBytesAtomically handle payload)
