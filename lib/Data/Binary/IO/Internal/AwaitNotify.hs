{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Binary.IO.Internal.AwaitNotify
  ( Await (..)
  , Notify (..)
  , newAwaitNotify
  )
where

import           Data.Word (Word8)
import qualified Foreign
import qualified System.IO as IO
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process

-- | Static pointer that points to a single 'Word8'
someWord8Ptr :: Foreign.Ptr Word8
someWord8Ptr = unsafePerformIO (Foreign.calloc @Word8)

{-# NOINLINE someWord8Ptr #-}

-- | Await signal from a paired 'Notify'. Returns 'False' if the paired 'Notify' does not exist
-- (any more).
newtype Await = Await
  { runAwait :: IO Bool }

-- | Notify the paired 'Await'.
newtype Notify = Notify
  { runNotify :: IO () }

-- | Create a pair of 'Await' and 'Notify.
newAwaitNotify :: IO (Await, Notify)
newAwaitNotify = do
  (read, write) <- Process.createPipe

  IO.hSetBuffering read IO.NoBuffering
  IO.hSetBuffering write IO.NoBuffering

  IO.hSetBinaryMode read True
  IO.hSetBinaryMode write True

  let notify = IO.hPutBuf write someWord8Ptr 1
  let await  = (> 0) <$> IO.hGetBufSome read someWord8Ptr 1

  pure (Await await, Notify notify)
