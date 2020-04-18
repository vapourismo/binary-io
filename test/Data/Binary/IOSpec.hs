module Data.Binary.IOSpec (spec) where

import Prelude hiding (read)

import Control.Monad.IO.Class
import Control.Monad

import Data.Binary.IO
import Data.Binary
import Data.Bifoldable

import Test.Hspec

import System.Process
import System.IO
import System.IO.Error

-- | Create a pipe with no buffering on read and write side.
createUnbufferedPipe :: IO (Handle, Handle)
createUnbufferedPipe = do
  handles <- createPipe
  join bitraverse_ (`hSetBuffering` NoBuffering) handles
  pure handles

-- | The 'Binary' instance of this type implements a 'get' that always fails
data BadGet

instance Binary BadGet where
  put = error "Not implemented"

  get = fail "get for BadGet will always"

spec :: Spec
spec = do
  describe "Reader" $ before createUnbufferedPipe $ do
    -- Test something with 0 length
    it "reads ()" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)
      write handleWrite ()
      () <- read reader
      pure ()

    -- Test something with fixed non-zero length
    it "reads Int" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)
      write handleWrite (1337 :: Int)
      1337 <- read reader :: IO Int
      pure ()

    -- Test something with variable length
    it "reads String" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)
      write handleWrite "Hello World"
      "Hello World" <- read reader
      pure ()

    -- When the read handle has reached its end, reading from it should not throw an error.
    -- However, no more input can be read therefore the underling 'Get' parser should fail.
    it "throws ReaderGetError when Handle is EOF" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)

      hClose handleWrite
      eof <- hIsEOF handleRead
      shouldBe eof True

      shouldThrow (read reader :: IO String) (\ReaderGetError{} -> True)

    -- Reading from a closed handle should throw. That exception needs to surface.
    it "throws IllegalOperation when Handle is closed" $ \(handleRead, _handleWrite) -> do
      reader <- liftIO (newReader handleRead)

      hClose handleRead
      closed <- hIsClosed handleRead
      shouldBe closed True

      shouldThrow (read reader :: IO String) isIllegalOperation

    -- Failing 'Get' operations should not advance the stream position.
    it "preserves the stream position when Get operation fails" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)

      write handleWrite "Hello World"
      shouldThrow (read reader :: IO BadGet) (\ReaderGetError{} -> True)
      "Hello World" <- read reader

      pure ()
