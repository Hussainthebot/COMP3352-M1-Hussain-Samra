module Main where

import Test.Hspec
import qualified C0PassesSpec
import qualified X86bPassesSpec
import qualified X86Passes.LivenessSpec

main :: IO ()
main = hspec $ do
  C0PassesSpec.spec
  X86bPassesSpec.spec
  X86Passes.LivenessSpec.spec