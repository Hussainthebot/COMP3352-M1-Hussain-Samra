module X86Passes.InterferenceGraphSpec (spec) where

import Test.Hspec
import X86Passes.InterferenceGraph
import X86Passes.Liveness
import Data.Set as S
import Graph.Graph as G
import X86b as X
import Parsers.NiParser
import CompilerPasses
import ComposePasses
import Data.List as L
import Env

-- a function to help deal with passes
n1StrToX86b :: String -> Result (X86b, [String])
n1StrToX86b str = do
  niParseStr str
    >>= uniquifyPass
    >>= removeComplexOperasPass
    >>= explicateControlPass
    >>= selectInstructionsPass


--uncoverLive :: X86b -> [(Label, Block, [Set.Set Arg])]

interfereTest :: String -> Result (Env (Graph X.Arg))
interfereTest str =
  case n1StrToX86b str of
    Right (x86b, locals) ->
        let argEnv = (L.foldl'
              (\env (Label lbl, blk, set) ->
                extendEnv lbl set env) makeEnv (uncoverLive x86b))
        in
          Right $ buildInterference x86b argEnv
    Left msg -> Left msg

spec :: Spec
spec = do
  describe "interferenceInstr tests on instruction interference: " $ do
    it "can make an empty interference graph" $ do
      instrInterference (Movq (Var "a") (Var "b")) S.empty G.empty
        `shouldBe` G.empty
    it "can see interference from move correctly where source and dest are not the same" $ do
      instrInterference (Movq (Var "a") (Var "b")) (S.fromList [Var "c"]) G.empty
        `shouldBe` G.fromList [(Var "b", [Var "c"]),(Var "c", [Var "b"])]
    it "can see interference from move correctly where dest is live" $ do
      instrInterference (Movq (Var "a") (Var "b")) (S.fromList [Var "b"]) G.empty
        `shouldBe` G.empty
    it "can see interference from move correctly where source is live" $ do
      instrInterference (Movq (Var "a") (Var "b")) (S.fromList [Var "a"]) G.empty
        `shouldBe` G.empty

  describe "interferenceBlock tests on block interference: " $ do
    it "can create the interference for a block" $ do
      interferenceBlock (Block
        [ Movq (Imm 5) (Var "a"),
          Movq (Imm 30) (Var "b"),
          Movq (Var "a") (Var "c"),
          Movq (Imm 10) (Var "b"),
          Addq (Var "b") (Var "c")
        ])
        (S.fromList <$>
        [ []
        , [Var "a"]
        , [Var "a"]
        , [Var "c"]
        , [Var "b",Var "c"]
        ])
        G.empty `shouldBe` G.fromList [(Var "a", [Var "b"]), (Var "c", [Var "b"]),
          (Var "b", [Var "a", Var "c"])]

  describe "buildInterference tests on whole programs: " $ do
    it "can create an environment of intereference graphs for a program" $ do
      interfereTest "let ni x is 5 in x end" `shouldBe`
        Right (Env [("start",G.fromList [(Reg RSP,[Var "s0",Reg RAX]),
          (Var "s0",[Reg RSP]),(Reg RAX,[Reg RSP])]),
          ("conclusion",G.fromList [(Reg RSP,[Reg RBP]),(Reg RBP,[Reg RSP])]),
          ("_main",G.fromList [])])
