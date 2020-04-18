module Main (main) where

import Test.Hspec

import qualified Data.Binary.IOSpec

main :: IO ()
main = hspec Data.Binary.IOSpec.spec