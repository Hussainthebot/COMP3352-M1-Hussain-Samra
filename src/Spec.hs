module Main where

import Test.Hspec
import qualified C0PassesSpec
import qualified X86bPassesSpec

main :: IO ()
main = hspec $ do
  C0PassesSpec.spec
  X86bPassesSpec.spec