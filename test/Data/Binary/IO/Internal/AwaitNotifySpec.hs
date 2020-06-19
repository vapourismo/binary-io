{-# LANGUAGE NumericUnderscores #-}
module Data.Binary.IO.Internal.AwaitNotifySpec (spec) where

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Binary.IO.Internal.AwaitNotify
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec =
  Hspec.describe "newAwaitNotify" $ do
    Hspec.it "can get notified single-threadedly" $ do
      (await, notify) <- newAwaitNotify

      runNotify notify

      result <- runAwait await
      result `Hspec.shouldBe` True

    Hspec.it "can get notified multi-threadedly" $ do
      (await, notify) <- newAwaitNotify

      _ <- forkIO $ do
        threadDelay 1_000_000
        runNotify notify

      result <- runAwait await
      result `Hspec.shouldBe` True

    Hspec.it "detects that Notify went out-of-scope" $ do
      (await, _notify) <- newAwaitNotify
      result <- runAwait await
      result `Hspec.shouldBe` False

    Hspec.it "can notify without a paired " $ do
      (_await, notify) <- newAwaitNotify
      runNotify notify
