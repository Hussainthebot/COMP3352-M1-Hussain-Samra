module ComposePasses where

import N1
import CompilerPasses
import NiPasses.N1Passes
import CPasses.C0Passes
import X86Passes.X86bPasses
import AssignHomes
import PatchInstructions

-- =====================================
-- PASS WRAPPERS
-- =====================================

-- Uniquify
uniquifyPass :: Program -> Result (Program, Integer)
uniquifyPass prog =
  case uniquify prog of
    CState (UState _ counter) (Right p') -> Right (p', counter)
    CState _ (Left msg)                  -> Left msg

-- Remove Complex Operands
removeComplexOperasPass :: (Program, Integer) -> Result Program
removeComplexOperasPass (prog, symCount) =
  case passRemoveComplexOperas (CState symCount (Right prog)) of
    CState _ (Right p') -> Right p'
    CState _ (Left msg) -> Left msg

-- Explicate Control
explicateControlPass :: Program -> Result ECPass
explicateControlPass = explicateControl

-- Instruction Selection
instructionSelectionPass :: ECPass -> Result SIPass
instructionSelectionPass (ECPass c0 locals) =
  instructionSelection c0 locals

-- Assign Homes
assignHomesPassWrapper :: SIPass -> Result AHPass
assignHomesPassWrapper (SIPass (x86prog, locals)) =
  assignHomesPass (x86prog, locals)

-- Patch Instructions
patchInstructionsPassWrapper :: AHPass -> Result PIPass
patchInstructionsPassWrapper (AHPass x86 locals extra) =
  patchInstructions x86

-- =====================================
-- FULL PIPELINE
-- =====================================

allPasses :: Program -> Result PIPass
allPasses p =
  uniquifyPass p
    >>= removeComplexOperasPass
    >>= explicateControlPass
    >>= instructionSelectionPass
    >>= assignHomesPassWrapper
    >>= patchInstructionsPassWrapper