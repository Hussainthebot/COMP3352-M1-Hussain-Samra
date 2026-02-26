-- src/X86bPasses/AssignHomes.hs
module AssignHomes
  ( AHPass(..)
  , assignHomesPass
  , zipVarWithStackLocations
  ) where

import CompilerPasses (Result)
import qualified Env
import Env (Env(..))
import X86b

-- carry the rewritten program + mapping + local count
data AHPass = AHPass
  { x86prog     :: X86b
  , env         :: Env Arg
  , localCount  :: Int
  } deriving (Eq, Show)

-- | Build a mapping from local variable names to stack locations.
--   Example: locals ["a","b"], base RBP, start -8, step 8
--   => [("a", Mem RBP (-8)), ("b", Mem RBP (-16))]
zipVarWithStackLocations :: [String] -> Reg -> Int -> Int -> [(String, Arg)]
zipVarWithStackLocations locals baseReg startOffset step =
  zip locals [ Mem baseReg (fromIntegral off) | off <- offsets ]
  where
    offsets = [startOffset, startOffset - step .. startOffset - step * (length locals - 1)]

-- | Entry point: replace all Var occurrences using stack locations.
assignHomesPass :: (X86b, [String]) -> Result AHPass
assignHomesPass (Program blocks, locals) =
  let bindings = zipVarWithStackLocations locals RBP (-8) 8
      env0     = foldr (\(k,v) acc -> Env.extendEnv k v acc) Env.makeEnv bindings
      nLocals  = length locals
  in do
    blocks' <- mapM (ahBlock env0) blocks
    pure $ AHPass (Program blocks') env0 nLocals

-- Process one labeled block
ahBlock :: Env Arg -> (Label, Block) -> Result (Label, Block)
ahBlock env0 (lbl, Block instrs) = do
  instrs' <- mapM (ahInstr env0) instrs
  pure (lbl, Block instrs')

-- Translate one instruction by rewriting its arguments
-- NOTE: we intentionally only cover the common instruction shapes.
-- complete the TODO cases below.
ahInstr :: Env Arg -> Instr -> Result Instr
ahInstr env0 ins =
  case ins of
    Addq a1 a2 -> Addq <$> ahArg env0 a1 <*> ahArg env0 a2
    Subq a1 a2 -> Subq <$> ahArg env0 a1 <*> ahArg env0 a2
    Movq a1 a2 -> Movq <$> ahArg env0 a1 <*> ahArg env0 a2
    Negq a     -> Negq <$> ahArg env0 a
    Pushq a    -> Pushq <$> ahArg env0 a
    Popq a     -> Popq <$> ahArg env0 a
    Jmp l      -> pure (Jmp l)
    Retq       -> pure Retq
    Callq l n  -> pure (Callq l n)

    -- TODO (optional, if your instruction set expands later):
    -- add any missing instruction forms here if your X86b has more

-- Replace a Var with its stack home; otherwise leave unchanged
ahArg :: Env Arg -> Arg -> Result Arg
ahArg env0 arg =
  case arg of
    Var name ->
      case Env.lookupEnv name env0 of
        Just home -> pure home
        Nothing   -> Left ("assignHomes: unbound variable " ++ show name)
    _ -> pure arg