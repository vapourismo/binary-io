{-# LANGUAGE DeriveAnyClass #-}

module Data.Binary.IOSpec (spec) where

import Prelude hiding (read)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (join)
import Control.Exception (Exception, throw)

import           Data.Typeable (typeOf)
import           Data.List (isInfixOf)
import           Data.Binary.IO
import           Data.Binary (Binary (..), decode)
import           Data.Bifoldable (bitraverse_)
import qualified Data.ByteString.Lazy as ByteString

import Test.Hspec (Spec, Expectation, before, describe, it, shouldBe, shouldThrow)

import System.Process (createPipe)
import System.IO (Handle, BufferMode (NoBuffering), hSetBuffering, hClose, hIsEOF, hIsClosed)
import System.IO.Error (isIllegalOperation, ioeGetErrorString)

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

data ExampleException = ExampleException
  deriving (Show, Exception)

-- | Check that a read from the 'Handle' yields the given value.
shouldRead :: (Show a, Eq a, Binary a) => Handle -> a -> Expectation
shouldRead handle expectedValue = do
  contents <- ByteString.hGetContents handle
  shouldBe (decode contents) expectedValue

spec :: Spec
spec = before createUnbufferedPipe $ do
  describe "Reader" $ do
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
    it "throws IllegalOperation when read Handle is closed" $ \(handleRead, _handleWrite) -> do
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

    -- Failing continuations should not advance the stream position.
    it "preserves the stream position when continuation fails" $ \(handleRead, handleWrite) -> do
      reader <- liftIO (newReader handleRead)

      write handleWrite "Hello World"
      shouldThrow (readWith reader (\() -> throw ExampleException)) (\ExampleException -> True)
      "Hello World" <- read reader

      pure ()

  describe "Writer" $ do
    let
      testWrites value =
        it ("writes " <> show (typeOf value)) $ \(handleRead, handleWrite) -> do
          let writer = newWriter handleWrite

          write writer value
          shouldRead handleRead value

    -- Test something with 0 length
    testWrites ()

    -- Test something with fixed non-zero length
    testWrites (1337 :: Int)

    -- Test something with variable length
    testWrites "Hello World"

    it "throws ResourceVanished when read Handle is closed" $ \(handleRead, handleWrite) -> do
      let writer = newWriter handleWrite

      hClose handleRead
      closed <- hIsClosed handleRead
      shouldBe closed True

      shouldThrow (write writer "Hello World") $ \exception ->
        isInfixOf "resource vanished" (ioeGetErrorString exception)

    it "throws IllegalOperation when write Handle is closed" $ \(_handleRead, handleWrite) -> do
      let writer = newWriter handleWrite

      hClose handleWrite
      closed <- hIsClosed handleWrite
      shouldBe closed True

      shouldThrow (write writer "Hello World") isIllegalOperation
