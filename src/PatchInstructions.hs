-- src/X86bPasses/PatchInstructions.hs
module PatchInstructions
  ( PIPass(..)
  , patchInstructions
  ) where

import CompilerPasses (Result)
import X86b

newtype PIPass = PIPass X86b
  deriving (Eq, Show)

patchInstructions :: X86b -> Result PIPass
patchInstructions (Program blocks) = do
  blocks' <- mapM piBlock blocks
  pure (PIPass (Program blocks'))

piBlock :: (Label, Block) -> Result (Label, Block)
piBlock (lbl, Block instrs) =
  pure (lbl, Block (concatMap piInstr instrs))

-- Rewrite one instruction into 0, 1, or 2 instructions
piInstr :: Instr -> [Instr]
piInstr ins =
  case ins of
    -- Remove redundant moves
    Movq src dst | src == dst -> []

    -- Fix mem-to-mem move via %rax
    Movq (Mem r1 o1) (Mem r2 o2) ->
      [ Movq (Mem r1 o1) (Reg RAX)
      , Movq (Reg RAX) (Mem r2 o2)
      ]

    -- Keep all other movq as-is
    Movq src dst -> [Movq src dst]

    -- TODO: patch mem-to-mem Addq/Subq if you decide to require it
    -- e.g. Addq (Mem ...) (Mem ...) is illegal on x86-64 in general

    -- Suggested TODO version (pick one):
    -- (A) Only patch Addq where dst is Mem and src is Mem:
    -- Addq (Mem r1 o1) (Mem r2 o2) -> ...

    -- (B) Patch Subq similarly:
    -- Subq (Mem r1 o1) (Mem r2 o2) -> ...

    -- Otherwise, pass through:
    Addq a b -> [Addq a b]
    Subq a b -> [Subq a b]
    Negq a   -> [Negq a]
    Pushq a  -> [Pushq a]
    Popq a   -> [Popq a]
    Callq l n-> [Callq l n]
    Jmp l    -> [Jmp l]
    Retq     -> [Retq]