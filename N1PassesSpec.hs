module N1PassesSpec (spec) where

import Test.Hspec
import Data.Int
import CompilerPasses
import N1Passes
import N1

uniquifyResult = getResult . uniquify
rcoResult prog = getResult (passRemoveComplexOperas (CState 0 (Right prog)))

spec :: Spec
spec = do
  describe "Uniqueify Tests:" $ do
    it "can uniqueify a simple let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (Program (Let "s0" (Int 5) (Var "s0")))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "y" (Int 5) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s0"))))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "y" (Int 5) (Var "y"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a shadowed name nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "x" (Int 5) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a nested let expression under Add" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Int 6) (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Int 6) (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Var "s0") (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Let "y" (Int 5) (Var "y")) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Let "s1" (Int 5) (Var "s1")) (Var "s0"))))

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (Program (Let "x" (Var "z") (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Left "Symbol 'z' not found"

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (Program (Let "x" (Var "x") (Add (Int 6) (Let "x" (Int 5) (Var "x")))))  `shouldBe`
        Left "Symbol 'x' not found"

    -- -------------------------------------------------------
    -- Additional tests
    -- -------------------------------------------------------

    -- A plain integer program needs no renaming
    it "uniqueifies a program with no variables (Int)" $ do
      uniquifyResult (Program (Int 42)) `shouldBe`
        Right (Program (Int 42))

    -- A Negate of an Int has no variables to rename
    it "uniqueifies Negate (Int n)" $ do
      uniquifyResult (Program (Negate (Int 3))) `shouldBe`
        Right (Program (Negate (Int 3)))

    -- An unbound variable at the top level is an error
    it "reports an error for a bare unbound variable" $ do
      uniquifyResult (Program (Var "oops")) `shouldBe`
        Left "Symbol 'oops' not found"

    -- Three-deep nesting; every counter step is distinct
    it "uniqueifies three deeply nested lets with distinct fresh names" $ do
      uniquifyResult
        (Program (Let "x" (Int 1)
                   (Let "y" (Int 2)
                     (Let "z" (Int 3)
                       (Add (Var "x") (Add (Var "y") (Var "z")))))))
        `shouldBe`
        Right (Program (Let "s0" (Int 1)
                         (Let "s1" (Int 2)
                           (Let "s2" (Int 3)
                             (Add (Var "s0") (Add (Var "s1") (Var "s2")))))))

    -- The binding expression of a Let cannot see the name being bound.
    -- Here the inner let tries to use "y" in its *binding* position;
    -- "y" is bound in the outer let, so it should resolve correctly,
    -- but the inner let's own name should NOT be visible in its own RHS.
    it "variable bound by outer let is in scope for inner let's binding expr" $ do
      uniquifyResult
        (Program (Let "y" (Int 10)
                   (Let "z" (Var "y")   -- Var "y" IS in scope here
                     (Var "z"))))
        `shouldBe`
        Right (Program (Let "s0" (Int 10)
                         (Let "s1" (Var "s0")
                           (Var "s1"))))

    -- Negate of a let-bound variable
    it "uniqueifies Negate of a bound variable" $ do
      uniquifyResult
        (Program (Let "x" (Int 7) (Negate (Var "x"))))
        `shouldBe`
        Right (Program (Let "s0" (Int 7) (Negate (Var "s0"))))

  -- =========================================================
  describe "Remove complex opera* pass tests:" $ do
    it "can rco on Ints" $ do
      rcoResult (Program (Int 7)) `shouldBe` Right (Program (Int 7))
    it "can rco on Read" $ do
      rcoResult (Program Read) `shouldBe` Right (Program Read)
    it "can rco on Var" $ do
      rcoResult (Program (Var "x")) `shouldBe` Right (Program (Var "x"))
    it "can rco on Negate Var" $ do
      rcoResult (Program (Negate (Var "x"))) `shouldBe` Right (Program (Negate (Var "x")))
    it "can rco on Negate Int" $ do
      rcoResult (Program (Negate (Int 6))) `shouldBe` Right (Program (Negate (Int 6)))
    it "can rco on Negate Negate Int" $ do
      rcoResult (Program (Negate (Negate (Int 6)))) `shouldBe` Right (Program (Let "s0" (Negate (Int 6)) (Negate (Var "s0"))))
    it "can rco on Negate Negate Var" $ do
      rcoResult (Program (Negate (Negate (Var "x")))) `shouldBe` Right (Program (Let "s0" (Negate (Var "x")) (Negate (Var "s0"))))
    it "can rco on Add Int Int" $ do
      rcoResult (Program (Add (Int 6) (Int 7))) `shouldBe` Right (Program (Add (Int 6) (Int 7)))
    it "can rco on Add Var Int" $ do
      rcoResult (Program (Add (Var "x") (Int 7))) `shouldBe` Right (Program (Add (Var "x") (Int 7)))
    it "can rco on Add Var (Negate Int)" $ do
      rcoResult (Program (Add (Var "x") (Negate (Int 7)))) `shouldBe` Right (Program (Let "s0" (Negate (Int 7)) (Add (Var "x") (Var "s0"))))
    it "can pass case on Add (Negate (Var \"x\")) (Negate (Int 7)))" $ do
      rcoResult (Program (Add (Negate (Var "x")) (Negate (Int 7)))) `shouldBe`
        Right (Program
                (Let "s0" (Negate (Var "x"))
                  (Let "s1" (Negate (Int 7))
                    (Add (Var "s0") (Var "s1")))))
    it "can rco pass case on: (Program (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8)))))" $ do
      rcoResult (Program (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8))))) `shouldBe`
        Right (Program (Let "s0" (Negate (Int 5))
                          (Let "s1" (Negate (Int 7))
                             (Let "s2" (Negate (Int 8))
                                (Let "s3" (Add (Var "s1") (Var "s2"))
                                  (Add (Var "s0") (Var "s3")))))))
    it "can rco on let x = 5 in x" $ do
      rcoResult (Program (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Var "x")))
    it "can rco on let x = -5 in x" $ do
      rcoResult (Program (Let "x" (Negate (Int 5)) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Negate (Int 5)) (Var "x")))
    it "can rco on let x = -5+3 in x" $ do
      rcoResult (Program (Let "x" (Add (Negate (Int 5)) (Int 3)) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Let "s0" (Negate (Int 5)) (Add (Var "s0") (Int 3))) (Var "x")))
    it "can rco on let x = 5 in -(x + x)" $ do
      rcoResult (Program (Let "x" (Int 5) (Negate (Add (Var "x") (Var "x"))))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Let "s0" (Add (Var "x") (Var "x")) (Negate (Var "s0")))))
    it "can rco on let x = 5 in -(x + -x)" $ do
      rcoResult (Program (Let "x" (Int 5) (Negate (Add (Var "x") (Negate (Var "x")))))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Let "s0" (Negate (Var "x")) (Let "s1" (Add (Var "x") (Var "s0")) (Negate (Var "s1"))))))

    -- -------------------------------------------------------
    -- Additional tests
    -- -------------------------------------------------------

    -- A bare Read is already atomic; nothing to lift
    it "rco leaves a bare Read unchanged" $ do
      rcoResult (Program Read) `shouldBe` Right (Program Read)

    -- Add of two Reads: both operands are atomic (Read is treated as
    -- atomic in rcoAtm), so no lifting needed at the expression level
    it "rco leaves Add Read Read unchanged (both operands atomic)" $ do
      rcoResult (Program (Add Read Read)) `shouldBe` Right (Program (Add Read Read))

    -- Negate of a Read is already atomic-argument form
    it "rco leaves Negate Read unchanged" $ do
      rcoResult (Program (Negate Read)) `shouldBe` Right (Program (Negate Read))

    -- A Let whose body is a complex Add must lift the Add's operands
    it "rco on let x = 5 in x + x (both operands already atomic)" $ do
      rcoResult (Program (Let "x" (Int 5) (Add (Var "x") (Var "x")))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Add (Var "x") (Var "x"))))

    -- Three nested negations; counter must advance for each lift
    it "rco on triple-nested Negate lifts two temporaries" $ do
      rcoResult (Program (Negate (Negate (Negate (Int 1))))) `shouldBe`
        Right (Program
                (Let "s0" (Negate (Int 1))
                  (Let "s1" (Negate (Var "s0"))
                    (Negate (Var "s1")))))

    -- Two separate Adds at the top level, both complex on one side
    it "rco on Add (Negate Int) (Negate Int) lifts both" $ do
      rcoResult (Program (Add (Negate (Int 2)) (Negate (Int 3)))) `shouldBe`
        Right (Program
                (Let "s0" (Negate (Int 2))
                  (Let "s1" (Negate (Int 3))
                    (Add (Var "s0") (Var "s1")))))

    -- Counter must continue from where the Let-binding expression left off
    it "rco on nested lets with complex bindings shares the counter" $ do
      rcoResult
        (Program
          (Let "x" (Add (Negate (Int 1)) (Int 2))
            (Let "y" (Add (Negate (Int 3)) (Int 4))
              (Add (Var "x") (Var "y"))))) `shouldBe`
        Right (Program
          (Let "x" (Let "s0" (Negate (Int 1)) (Add (Var "s0") (Int 2)))
            (Let "y" (Let "s1" (Negate (Int 3)) (Add (Var "s1") (Int 4)))
              (Add (Var "x") (Var "y")))))