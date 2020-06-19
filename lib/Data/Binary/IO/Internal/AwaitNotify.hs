{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Binary.IO.Internal.AwaitNotify
  ( Await (..)
  , Notify (..)
  , newAwaitNotify
  )
where

import qualified Foreign

import           Data.Word (Word8)
import qualified System.IO as IO
import qualified System.Process as Process

-- | Await signal from a paired 'Notify'. Returns 'False' if the paired 'Notify' does not exist
-- (any more).
newtype Await = Await
  { runAwait :: IO Bool }

-- | Notify the paired 'Await'.
newtype Notify = Notify
  { runNotify :: IO () }

newAwaitNotify :: IO (Await, Notify)
newAwaitNotify = do
  buf <- Foreign.calloc @Word8
  (read, write) <- Process.createPipe

  IO.hSetBuffering read IO.NoBuffering
  IO.hSetBuffering write IO.NoBuffering

  IO.hSetBinaryMode read True
  IO.hSetBinaryMode write True

  let notify = IO.hPutBuf write buf 1
  let await  = (> 0) <$> IO.hGetBufSome read buf 1

  pure (Await await, Notify notify)
