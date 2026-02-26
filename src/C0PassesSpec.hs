module C0PassesSpec (spec) where

import Test.Hspec
import Data.Int
import CompilerPasses
import CPasses.C0Passes
import qualified CPasses.C0 as C0
import N1

spec :: Spec
spec = do
  describe "Explicate Control Tests:" $ do
    it "can explicate control of simple ints" $ do
      explicateControl (N1.Program (N1.Int 5)) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return $ C0.Atm $ C0.Int 5)]) [])

    it "can explicate control of simple vars" $ do
      explicateControl (N1.Program (N1.Var "x")) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return $ C0.Atm $ C0.Var "x")]) [])

    it "can explicate control of negate var" $ do
      explicateControl (N1.Program (Negate (N1.Var "x"))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Sub $ C0.Var "x"))]) [])

    it "can explicate control of negate int" $ do
      explicateControl (N1.Program (Negate (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Sub $ C0.Int 6))]) [])

    it "can explicate control of add int int" $ do
      explicateControl (N1.Program (N1.Add (N1.Int 5) (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Add (C0.Int 5) (C0.Int 6)))]) [])

    it "can explicate control of add int var" $ do
      explicateControl (N1.Program (N1.Add (N1.Int 5) (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Add (C0.Int 5) (C0.Var "y")))]) [])

    it "can explicate control of add var int" $ do
      explicateControl (N1.Program (N1.Add (N1.Var "x") (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Add (C0.Var "x") (C0.Int 6)))]) [])

    it "can explicate control of add var var" $ do
      explicateControl (N1.Program (N1.Add (N1.Var "x") (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Return (C0.Add (C0.Var "x") (C0.Var "y")))]) [])

    it "can explicate control of a simple let assignment of int" $ do
      explicateControl (N1.Program (Let "x" (N1.Int 5) (N1.Var "x"))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Seq (C0.Assign "x" (C0.Atm (C0.Int 5))) (C0.Return (C0.Atm (C0.Var "x"))))]) ["x"])

    it "can explicate control of a simple let assignment with add as body" $ do
      explicateControl (N1.Program (Let "x" (N1.Int 5) (N1.Add (N1.Var "x") (N1.Int 6)))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Seq (C0.Assign "x" (C0.Atm (C0.Int 5))) (C0.Return (C0.Add (C0.Var "x") (C0.Int 6))))]) ["x"])

    it "can explicate control of a simple let assignment with sub as body" $ do
      explicateControl (N1.Program (Let "x" (N1.Int 5) (N1.Negate (N1.Var "x")))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Seq (C0.Assign "x" (C0.Atm (C0.Int 5))) (C0.Return (C0.Sub (C0.Var "x"))))]) ["x"])

    it "can explicate control of a nested let assignment" $ do
      explicateControl (N1.Program (Let "x" (N1.Int 5) (Let "y" (N1.Int 6) (N1.Add (N1.Var "x") (N1.Var "y"))))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Seq (C0.Assign "x" (C0.Atm (C0.Int 5)))
                                               (C0.Seq (C0.Assign "y" (C0.Atm (C0.Int 6)))
                                                  (C0.Return (C0.Add (C0.Var "x") (C0.Var "y")))))]) ["x", "y"])

    it "can explicate control of a nested lets with add as body" $ do
      explicateControl (N1.Program (Let "y"
                                     (Let "x1" (N1.Int 20)
                                       (Let "x2" (N1.Int 22)
                                         (N1.Add (N1.Var "x1") (N1.Var "x2")))) (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", C0.Seq (C0.Assign "x1" (C0.Atm (C0.Int 20)))
                                              (C0.Seq (C0.Assign "x2" (C0.Atm (C0.Int 22)))
                                                (C0.Seq (C0.Assign "y" (C0.Add (C0.Var "x1") (C0.Var "x2")))
                                                  (C0.Return (C0.Atm (C0.Var "y"))))))]) ["x1","x2","y"])

    -- REQUIRED ADDITIONAL TEST 1 (normal)
    it "explicateControl simple let add (extra test)" $ do
      explicateControl (N1.Program (Let "x" (N1.Int 10) (N1.Add (N1.Var "x") (N1.Int 3)))) `shouldBe`
        Right (ECPass (C0.Program [("start",
          C0.Seq (C0.Assign "x" (C0.Atm (C0.Int 10)))
              (C0.Return (C0.Add (C0.Var "x") (C0.Int 3))))]) ["x"])

    -- REQUIRED ADDITIONAL TEST 2 (edge nested case)
    it "explicateControl nested let deeper (extra test)" $ do
      explicateControl (N1.Program (Let "a" (N1.Int 1)
        (Let "b" (N1.Int 2)
          (Let "c" (N1.Int 3)
            (N1.Add (N1.Var "a") (N1.Var "c")))))) `shouldBe`
        Right (ECPass (C0.Program [("start",
          C0.Seq (C0.Assign "a" (C0.Atm (C0.Int 1)))
            (C0.Seq (C0.Assign "b" (C0.Atm (C0.Int 2)))
              (C0.Seq (C0.Assign "c" (C0.Atm (C0.Int 3)))
                (C0.Return (C0.Add (C0.Var "a") (C0.Var "c"))))))]) ["a","b","c"])