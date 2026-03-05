module X86Passes.X86bPasses where

import qualified X86b as X
import CompilerPasses
import CPasses.C0

data SIPass = SIPass (X.X86b, [String])
  deriving (Eq, Show)

-- Original instruction selection (no prelude/conclusion)
instructionSelection :: C0 -> [String] -> Result SIPass
instructionSelection (CPasses.C0.Program blocks) locals =
  let blocks' = map (\(name, t) -> (X.Label name, X.Block (selectTail t))) blocks
  in Right (SIPass (X.Program blocks', locals))

-- Instruction selection with prelude and conclusion blocks prepended
instructionSelectionWithPrelude :: C0 -> [String] -> Result SIPass
instructionSelectionWithPrelude (CPasses.C0.Program blocks) locals =
  let prelude    = (X.Label "_main", X.Block
                    [ X.Pushq (X.Reg X.RBP)
                    , X.Movq (X.Reg X.RSP) (X.Reg X.RBP)
                    , X.Jmp (X.Label "start")])
      conclusion = (X.Label "conclusion", X.Block
                    [ X.Popq (X.Reg X.RBP)
                    , X.Retq ])
      blocks' = map (\(name, t) -> (X.Label name, X.Block (selectTail t))) blocks
  in Right (SIPass (X.Program (prelude : conclusion : blocks'), locals))

-- Convert C0 atoms to x86 args
atmToArg :: CPasses.C0.Atom -> X.Arg
atmToArg a =
  case a of
    CPasses.C0.Int n -> X.Imm (fromIntegral n)
    CPasses.C0.Var x -> X.Var x

-- Generate instructions that compute Exp into destination
selectExp :: CPasses.C0.Exp -> X.Arg -> [X.Instr]
selectExp e dst =
  case e of
    Atm a ->
      [X.Movq (atmToArg a) dst]

    Add a b ->
      [ X.Movq (atmToArg a) dst
      , X.Addq (atmToArg b) dst
      ]

    Sub a ->
      [ X.Movq (atmToArg a) dst
      , X.Negq dst
      ]

    Read ->
      [ X.Callq (X.Label "read_int") 0
      , X.Movq (X.Reg X.RAX) dst
      ]

-- Walk tail and emit instructions
selectTail :: Tail -> [X.Instr]
selectTail t =
  case t of
    Seq (Assign x e) rest ->
      selectExp e (X.Var x) ++ selectTail rest

    Return e ->
      selectExp e (X.Reg X.RAX) ++ [X.Jmp (X.Label "conclusion")]