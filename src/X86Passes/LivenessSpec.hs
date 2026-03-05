module X86Passes.LivenessSpec (spec) where

import Test.Hspec
import X86Passes.Liveness
-- import X86Passes.SelectInstructions
-- import NiPasses.Uniquify
-- import NiPasses.RemoveComplexOperas
-- import CPasses.ExplicateControl
import ComposePasses
import X86b
import X86Passes.X86bPasses (SIPass(..))
import Env
import Parsers.NiParser
import CompilerPasses
import qualified Data.Set as Set

-- a function to help deal with passes
n1StrToX86b :: String -> Result (X86b, [String])
n1StrToX86b str = do
  p  <- niParseString str
  p1 <- uniquifyPass p
  p2 <- removeComplexOperasPass p1
  p3 <- explicateControlPass p2
  (SIPass (x86, locals)) <- instructionSelectionPass p3
  return (x86, locals)

liveTest :: String -> Result [(Label, Block, [Set.Set X86b.Arg])]
liveTest str =
  case n1StrToX86b str of
    Right (x86b, _) -> Right $ uncoverLive x86b
    Left msg -> Left msg

simplifyResult :: [(Label, Block, [Set.Set X86b.Arg])] -> [(String, [Set.Set X86b.Arg])]
simplifyResult = map (\(Label lbl, _, lst) -> (lbl, lst))

makeProg lbl ins = Program [(Label lbl, Block ins)]

ex1 = "\n\
\let ni a is 5 in\n\
\  let ni b is 30 in\n\
\     let ni c is a in\n\
\       let ni b is 10 in\n\
\         b + c\n\
\       end\n\
\     end\n\
\  end\n\
\end\n"

spec :: Spec
spec = do
  describe "Liveness tests for read/write sets for an instruction:" $ do
    it "returns the correct read/write set for movq" $ do
      getLocs (Movq (Imm 5) (Var "a")) `shouldBe` RW (Set.fromList []) (Set.fromList [Var "a"])

    it "returns the correct read/write set for movq with 2 vars" $ do
      getLocs (Movq (Var "b") (Var "a")) `shouldBe` RW (Set.fromList [Var "b"]) (Set.fromList [Var "a"])

    it "returns the correct read/write set for addq with 2 vars" $ do
      getLocs (Addq (Var "b") (Var "a")) `shouldBe` RW (Set.fromList [Var "b",Var "a"]) (Set.fromList [Var "a"])

    it "returns the correct read/write set for subq with 2 vars" $ do
      getLocs (Subq (Var "b") (Var "a")) `shouldBe` RW (Set.fromList [Var "b",Var "a"]) (Set.fromList [Var "a"])

    it "returns the correct read/write set for negq with 1 vars" $ do
      getLocs (Negq (Var "b")) `shouldBe` RW (Set.fromList [Var "b"]) (Set.fromList [Var "b"])

    it "returns the correct read/write set for retq" $ do
      getLocs Retq `shouldBe` RW (Set.fromList [Reg RSP]) (Set.fromList [Reg RSP])

    it "returns the correct read/write set for popq" $ do
      getLocs (Popq (Var "b")) `shouldBe` RW (Set.fromList [Reg RSP]) (Set.fromList [Reg RSP, Var "b"])

    it "returns the correct read/write set for pushq" $ do
      getLocs (Pushq (Var "b")) `shouldBe` RW (Set.fromList [Reg RSP, Var "b"]) (Set.fromList [Reg RSP])

  -- these test generating the live after sets
  describe "liveAfter tests for a given instruction and its next instruction: " $ do
    it "calculates the empty after set for a regular instruction with nothing following" $ do
      liveAfter (Movq (Var "a") (Var "b")) Set.empty `shouldBe` Set.empty
    it "calculates some other set for a regular instruction with that set following" $ do
      liveAfter (Subq (Imm 6) (Var "a")) (Set.fromList [Var "c"]) `shouldBe` Set.fromList [Var "c"]

  -- these test generating the live before sets
  describe "lifeBefore tests for a given instruction and its after set: " $ do
    it "calculates the empty before set with an empty after set" $ do
      liveBefore makeEnv (Movq (Imm 5) (Var "b")) Set.empty `shouldBe` Set.empty

    it "calculates the {a} with an empty after set and movq a, b" $ do
      liveBefore makeEnv (Movq (Var "a") (Var "b")) Set.empty `shouldBe` Set.fromList [Var "a"]

    it "calculates the {b,c} with an empty after set and addq a, b" $ do
      liveBefore makeEnv (Addq (Var "b") (Var "c")) Set.empty `shouldBe` Set.fromList [Var "b", Var "c"]

    it "calculates {c} with {b,c} in the after set and movq 10, b (i.e., eliminates b)" $ do
      liveBefore makeEnv (Movq (Imm 10) (Var "b")) (Set.fromList [Var "b", Var "c"])
        `shouldBe` Set.fromList [Var "c"]

    it "calculates {a} with {c} in the after set and movq a, c (i.e., eliminates c)" $ do
      liveBefore makeEnv (Movq (Var "a") (Var "c")) (Set.fromList [Var "c"])
        `shouldBe` Set.fromList [Var "a"]

    it "calculates {a} with {a} in the after set and movq 30, b (i.e., a is still live)" $ do
      liveBefore makeEnv (Movq (Imm 30) (Var "b")) (Set.fromList [Var "a"])
        `shouldBe` Set.fromList [Var "a"]

    it "calculates {} with {c} in the after set and movq 5, a (i.e., eliminates a)" $ do
      liveBefore makeEnv (Movq (Imm 5) (Var "a")) (Set.fromList [Var "a"])
        `shouldBe` Set.empty

    it "calculates the jump correctly with a set" $ do
      liveBefore (Env [("conclusion", [Set.fromList [Var "a"], Set.fromList [Var "b", Var "c"]])])
        (Jmp (Label "conclusion")) (Set.fromList [Var "b"]) `shouldBe` Set.fromList [Var "a"]

    it "calculates the jump correctly without a set" $ do
      liveBefore makeEnv
        (Jmp (Label "conclusion")) (Set.fromList [Var "b"]) `shouldBe` Set.empty

    it "return the liveness sets for a simple x86b program" $ do
      let res = simplifyResult $ uncoverLive (makeProg "_main" [ Movq (Imm 5) (Var "a")
                                                               , Movq (Imm 30) (Var "b")
                                                               , Movq (Var "a") (Var "c")
                                                               , Movq (Imm 10) (Var "b")
                                                               , Addq (Var "b") (Var "c")])
      res `shouldBe` [("_main", [ Set.fromList [], Set.fromList [Var "a"], Set.fromList [Var "a"]
                                , Set.fromList [Var "c"], Set.fromList [Var "b", Var "c"]
                                , Set.fromList []])]

    it "return the liveness sets for a simple x86b program" $ do
      let res = simplifyResult $ uncoverLive (makeProg "_main" [ Movq (Var "k") (Var "a")
                                                               , Movq (Imm 30) (Var "b")
                                                               , Movq (Var "a") (Var "c")
                                                               , Movq (Imm 10) (Var "b")
                                                               , Addq (Var "b") (Var "c")])
      res `shouldBe` [("_main", [ Set.fromList [Var "k"], Set.fromList [Var "a"], Set.fromList [Var "a"]
                                , Set.fromList [Var "c"], Set.fromList [Var "b", Var "c"]
                                , Set.fromList []])]


    it "return the liveness sets for a simple ni program" $ do
      let Right res = liveTest ex1
      print $ n1StrToX86b ex1
      simplifyResult res
        `shouldBe`
            [ ("_main", [ Set.fromList[Reg RSP, Reg RBP], Set.fromList [Reg RSP], Set.fromList [], Set.fromList []])
            , ("conclusion", [ Set.fromList [Reg RSP], Set.fromList [Reg RSP], Set.fromList []])
            , ("start", [ Set.fromList [Reg RSP]
                        , Set.fromList [Reg RSP, Var "s0"], Set.fromList [Reg RSP, Var "s0"]
                        , Set.fromList [Reg RSP, Var "s2"], Set.fromList [Reg RSP, Var "s2", Var "s3"]
                        , Set.fromList [Reg RSP, Reg RAX, Var "s2"], Set.fromList [Reg RSP]
                        , Set.fromList []])]