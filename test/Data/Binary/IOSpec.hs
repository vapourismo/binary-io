{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Binary.IOSpec (spec) where

import Prelude hiding (read)

import           Control.Exception (Exception, throw)
import           Control.Monad (join)
import           Data.Bifoldable (bitraverse_)
import           Data.Binary (Binary (..), encode)
import           Data.Binary.IO
import           Data.Binary.Put (putByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Foldable (for_)
import           Data.IORef (atomicModifyIORef', newIORef, readIORef)
import           Data.List (isInfixOf)
import           Data.Typeable (typeOf)
import qualified System.IO as IO
import           System.IO.Error (ioeGetErrorString, isIllegalOperation)
import           System.Process (createPipe)
import qualified Test.Hspec as Hspec

-- | Create a pipe with no buffering on read and write side.
createUnbufferedPipe :: IO (IO.Handle, IO.Handle)
createUnbufferedPipe = do
  handles <- createPipe
  join bitraverse_ (`IO.hSetBuffering` IO.NoBuffering) handles
  pure handles

-- | The 'Binary' instance of this type implements a 'get' that always fails
data BadGet

instance Binary BadGet where
  put = error "Not implemented"

  get = fail "get for BadGet will always"

data ExampleException = ExampleException
  deriving (Show, Exception)

-- | Check that a read from the 'IO.Handle' yields the given value.
shouldRead :: (Show a, Eq a, Binary a) => Reader -> a -> Hspec.Expectation
shouldRead reader expectedValue = do
  value <- read reader
  Hspec.shouldBe value expectedValue

-- | Close a handle and verify.
closeHandle :: IO.Handle -> Hspec.Expectation
closeHandle handle = do
  IO.hClose handle
  closed <- IO.hIsClosed handle
  Hspec.shouldBe closed True

withReader :: Hspec.SpecWith (Reader, IO.Handle) -> Hspec.SpecWith a
withReader = Hspec.aroundWith $ \run _ -> do
  (handleRead, handleWrite) <- createUnbufferedPipe

  reader <- newReader handleRead
  run (reader, handleWrite)

  readerWith <- newReaderWith (ByteString.hGetSome handleRead 1024)
  run (readerWith, handleWrite)

withReaderAndWriter :: Hspec.SpecWith (Reader, Writer) -> Hspec.SpecWith a
withReaderAndWriter =
  Hspec.aroundWith $ \run _ ->
    for_ readerMakers $ \mkReader -> for_ writerMakers $ \mkWriter -> do
      (handleRead, handleWrite) <- createUnbufferedPipe

      reader <- mkReader handleRead
      let writer = mkWriter handleWrite

      run (reader, writer)
  where
    readerMakers = [newReader, \handle -> newReaderWith (ByteString.hGetSome handle 1024)]

    writerMakers = [newWriter, newWriterWith . ByteString.hPut]

withPipe :: Hspec.SpecWith (IO.Handle, IO.Handle) -> Hspec.Spec
withPipe = Hspec.before createUnbufferedPipe

spec :: Hspec.Spec
spec = Hspec.parallel $ do
  Hspec.describe "Reader" $ do
    let
      testReads value =
        Hspec.it ("reads " <> show (typeOf value)) $ \(reader, handleWrite) -> do
          write handleWrite value
          shouldRead reader value

          write handleWrite value
          write handleWrite value

          shouldRead reader value
          shouldRead reader value

    withReader $ do
      -- Test something with 0 length
      testReads ()

      -- Test something with fixed non-zero length
      testReads (1337 :: Int)

      -- Test something with variable length
      testReads "Hello World"

    Hspec.it "recovers from exception in Get operation" $ do
      reader <- newReaderWith (pure (ByteString.Lazy.toStrict (encode "Hello World")))

      Hspec.shouldThrow (runGet reader (throw ExampleException)) (\ExampleException -> True)

      "Hello World" <- read reader

      pure ()

    withPipe $ do
      -- When the read handle has reached its end, reading from it should not throw an error.
      -- However, no more input can be read therefore the underling 'Get' parser should fail.
      Hspec.it "throws ReaderGetError when Handle is EOF" $ \(handleRead, handleWrite) -> do
        reader <- newReader handleRead

        IO.hClose handleWrite
        eof <- IO.hIsEOF handleRead
        Hspec.shouldBe eof True

        Hspec.shouldThrow (read reader :: IO String) (\ReaderGetError{} -> True)

      -- Reading from a closed handle should throw. That exception needs to surface.
      Hspec.it "throws IllegalOperation when read Handle is closed" $ \(handleRead, _handleWrite) -> do
        reader <- newReader handleRead

        closeHandle handleRead

        Hspec.shouldThrow (read reader :: IO String) isIllegalOperation

      -- Failing 'Get' operations should not advance the stream position.
      Hspec.it "preserves the stream position when Get operation fails" $ \(handleRead, handleWrite) -> do
        reader <- newReader handleRead

        write handleWrite "Hello World"
        Hspec.shouldThrow (read reader :: IO BadGet) (\ReaderGetError{} -> True)
        "Hello World" <- read reader

        pure ()

  Hspec.describe "Writer" $ do
    let
      testWrites value =
        Hspec.it ("writes " <> show (typeOf value)) $ \(reader, writer) -> do
          write writer value
          shouldRead reader value

          write writer value
          write writer value

          shouldRead reader value
          shouldRead reader value

    withReaderAndWriter $ do
      -- Test something with 0 length
      testWrites ()

      -- Test something with fixed non-zero length
      testWrites (1337 :: Int)

      -- Test something with variable length
      testWrites "Hello World"

    withPipe $ do
      Hspec.it "throws ResourceVanished when read Handle is closed" $ \(handleRead, handleWrite) -> do
        let writer = newWriter handleWrite

        closeHandle handleRead

        Hspec.shouldThrow (write writer "Hello World") $ \exception ->
          isInfixOf "resource vanished" (ioeGetErrorString exception)

      Hspec.it "throws IllegalOperation when write Handle is closed" $ \(_handleRead, handleWrite) -> do
        let writer = newWriter handleWrite

        closeHandle handleWrite

        Hspec.shouldThrow (write writer "Hello World") isIllegalOperation

    Hspec.it "preserves atomicity" $ do
      chunksRef <- newIORef []

      let
        writer = newWriterWith $ \chunk -> atomicModifyIORef' chunksRef $ \chunks ->
          (chunks ++ [chunk], ())

      runPut writer $ putByteString $ ByteString.Char8.pack "Hello"
      runPut writer $ putByteString $ ByteString.Char8.pack "World"
      runPut writer $ do
        putByteString $ ByteString.Char8.pack "Hello"
        putByteString $ ByteString.Char8.pack "World"

      chunks <- readIORef chunksRef
      chunks `Hspec.shouldBe` map ByteString.Char8.pack ["Hello", "World", "HelloWorld"]

  Hspec.describe "Pipe" $ do
    Hspec.it "is connected" $ do
      (reader, writer) <- newPipe

      write writer "Hello World"
      "Hello World" <- read reader

      empty <- isEmpty reader
      empty `Hspec.shouldBe` True

    Hspec.it "ends if writer is out of scope" $ do
      (reader, _writer) <- newPipe

      isEmpty <- isEmpty reader
      Hspec.shouldBe isEmpty True
