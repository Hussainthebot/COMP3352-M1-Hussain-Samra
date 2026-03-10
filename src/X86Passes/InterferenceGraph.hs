module X86Passes.InterferenceGraph where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as L

import X86b
import Env
import qualified Graph.Graph as G
import X86Passes.Liveness (getLocs, writeSet)

--------------------------------------------------------------------------------
-- helpers

isReal :: Arg -> Bool
isReal (Imm _) = False
isReal _       = True

--------------------------------------------------------------------------------
-- instruction interference

instrInterference :: Instr -> Set Arg -> G.Graph Arg -> G.Graph Arg
instrInterference instr liveAfter g =
  case instr of

    -- special rule for mov
    Movq s d ->
      if not (isReal d)
        then g
        else
          let neighbors =
                filter (\v -> isReal v && v /= d && v /= s)
                       (Set.toList liveAfter)
          in L.foldl' (\gr v -> G.addUndirectedEdge d v gr) g neighbors

    -- all other instructions
    _ ->
      let writes =
            filter isReal (Set.toList (writeSet (getLocs instr)))
      in L.foldl'
           (\gr d ->
              let neighbors =
                    filter (\v -> isReal v && v /= d)
                           (Set.toList liveAfter)
              in L.foldl' (\gr2 v -> G.addUndirectedEdge d v gr2) gr neighbors
           )
           g
           writes

--------------------------------------------------------------------------------
-- block graph

interferenceBlock :: Block -> [Set Arg] -> G.Graph Arg -> G.Graph Arg
interferenceBlock (Block instrs) liveAfters g0 =
  L.foldl'
    (\g (instr, liveA) -> instrInterference instr liveA g)
    g0
    (zip instrs liveAfters)

--------------------------------------------------------------------------------
-- program graph

buildInterference :: X86b -> Env [Set Arg] -> Env (G.Graph Arg)
buildInterference (Program blocks) liveEnv =
  L.foldl'
    (\envGraphs (Label lbl, blk) ->
        case lookupEnv lbl liveEnv of
          Just liveSets ->
            let g0 = G.empty
                g  = interferenceBlock blk liveSets g0
            in extendEnv lbl g envGraphs
          Nothing -> envGraphs
    )
    makeEnv
    blocks