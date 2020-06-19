module Main (main) where

import Test.Hspec

import qualified Data.Binary.IO.Internal.AwaitNotifySpec
import qualified Data.Binary.IOSpec

main :: IO ()
main = hspec $ do
  describe "Data" $ describe "Binary" $ describe "IO" $ do
    describe "Internal" $ describe "AwaitNotify"
      Data.Binary.IO.Internal.AwaitNotifySpec.spec
    Data.Binary.IOSpec.spec
