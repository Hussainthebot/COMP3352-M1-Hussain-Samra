{-# OPTIONS_GHC -Wall #-}
module X86Passes.Liveness
  ( ReadWrite(..)
  , SrcDst(..)
  , getRWs
  , getLocs
  , liveAfter
  , liveBefore
  , liveSets
  , uncoverLiveBlock
  , uncoverLive
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (partition)

import Env (Env)
import qualified Env

import X86b
  ( Arg(..), Instr(..), Block(..), X86b(..), Label(..), Reg(..) )

--------------------------------------------------------------------------------
-- Read/Write sets

data ReadWrite = RW
  { readSet  :: Set Arg
  , writeSet :: Set Arg
  } deriving (Eq, Show)

instance Semigroup ReadWrite where
  RW r1 w1 <> RW r2 w2 = RW (Set.union r1 r2) (Set.union w1 w2)

instance Monoid ReadWrite where
  mempty = RW Set.empty Set.empty

data SrcDst = Src | Dst | SrcDst deriving (Eq, Show)

getRWs :: SrcDst -> Arg -> ReadWrite
getRWs _    (Imm _) = mempty
getRWs role a =
  case role of
    Src    -> RW (Set.singleton a) Set.empty
    Dst    -> RW Set.empty (Set.singleton a)
    SrcDst -> RW (Set.singleton a) (Set.singleton a)

--------------------------------------------------------------------------------
-- Instruction-level R/W

callerSaved :: [Reg]
callerSaved = [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]

argRegs :: [Reg]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

getLocs :: Instr -> ReadWrite
getLocs (Movq src dst) =
  getRWs Src src <> getRWs Dst dst

getLocs (Addq src dst) =
  getRWs Src src <> getRWs SrcDst dst

getLocs (Subq src dst) =
  getRWs Src src <> getRWs SrcDst dst

getLocs (Negq dst) =
  getRWs SrcDst dst

getLocs (Pushq a) =
  RW (Set.fromList [Reg RSP, a]) (Set.singleton (Reg RSP))

getLocs (Popq a) =
  RW (Set.singleton (Reg RSP)) (Set.fromList [Reg RSP, a])

getLocs Retq =
  RW (Set.singleton (Reg RSP)) (Set.singleton (Reg RSP))

getLocs (Jmp _) =
  mempty

getLocs (Callq _ arity) =
  let rs = Set.fromList (map Reg (take (fromInteger arity) argRegs))
      ws = Set.fromList (map Reg callerSaved)
  in RW rs ws

--------------------------------------------------------------------------------
-- Live sets

liveAfter :: Instr -> Set Arg -> Set Arg
liveAfter _ after = after

liveBefore :: Env [Set Arg] -> Instr -> Set Arg -> Set Arg
liveBefore env instr after =
  case instr of
    Jmp (Label lbl) ->
      case Env.lookupEnv lbl env of
        Just (entrySet : _) -> entrySet
        _                   -> Set.empty
    _ ->
      let RW r w = getLocs instr
      in Set.union r (Set.difference after w)

liveSets :: Env [Set Arg] -> [Instr] -> [Set Arg]
liveSets env instrs =
  snd $ foldr step (Set.empty, [Set.empty]) instrs
  where
    step :: Instr -> (Set Arg, [Set Arg]) -> (Set Arg, [Set Arg])
    step instr (after, acc) =
      let before = liveBefore env instr after
      in (before, before : acc)

--------------------------------------------------------------------------------
-- Block/program level

uncoverLiveBlock :: Env [Set Arg] -> Block -> [Set Arg]
uncoverLiveBlock env (Block instrs) = liveSets env instrs

uncoverLive :: X86b -> [(Label, Block, [Set Arg])]
uncoverLive (Program blocks) =
  let (mains, rest) = partition (\(Label l, _) -> l == "_main") blocks
      blocks' = mains ++ rest

      emptyEnv :: Env [Set Arg]
      emptyEnv = foldr (\(Label l, _) e -> Env.extendEnv l [] e)
                       Env.makeEnv blocks'

      step :: Env [Set Arg] -> Env [Set Arg]
      step env =
        foldl
          (\e (Label l, blk) ->
              Env.extendEnv l (uncoverLiveBlock env blk) e)
          Env.makeEnv
          blocks'

      iterateToFixpoint :: Env [Set Arg] -> Env [Set Arg]
      iterateToFixpoint env =
        let env' = step env
        in if env' == env then env else iterateToFixpoint env'

      finalEnv = iterateToFixpoint emptyEnv

  in map (\(lbl@(Label l), blk) ->
            if l == "_main"
              then (lbl, blk, uncoverLiveBlock emptyEnv blk)
              else (lbl, blk, uncoverLiveBlock finalEnv blk))
         blocks'