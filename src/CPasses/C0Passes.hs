module CPasses.C0Passes where

import CPasses.C0
import qualified N1
import CompilerPasses

data ECPass = ECPass { prog :: C0, locals :: [String] }
  deriving (Eq, Show)

-- =====================================================
-- Top Level
-- =====================================================

explicateControl :: N1.Program -> Result ECPass
explicateControl (N1.Program expr) = do
  (ctail, ls) <- explicateTail expr
  pure $ ECPass (CPasses.C0.Program [("start", ctail)]) ls

-- =====================================================
-- Helpers
-- =====================================================

toAtom :: N1.Exp -> Result Atom
toAtom e =
  case e of
    N1.Int n -> Right (CPasses.C0.Int (fromIntegral n))
    N1.Var x -> Right (CPasses.C0.Var x)
    _        -> Left ("explicateControl: expected atom, got " ++ show e)

-- =====================================================
-- Tail Position
-- =====================================================

explicateTail :: N1.Exp -> Result (Tail, [String])
explicateTail e =
  case e of
    N1.Int n ->
      Right (Return (Atm (CPasses.C0.Int (fromIntegral n))), [])

    N1.Var x ->
      Right (Return (Atm (CPasses.C0.Var x)), [])

    N1.Read ->
      Right (Return Read, [])

    N1.Negate a -> do
      a' <- toAtom a
      Right (Return (Sub a'), [])

    N1.Add a b -> do
      a' <- toAtom a
      b' <- toAtom b
      Right (Return (Add a' b'), [])

    N1.Let x rhs body -> do
      (bodyTail, bodyLocals) <- explicateTail body
      (fullTail, rhsLocals)  <- explicateAssign x rhs bodyTail
      Right (fullTail, rhsLocals ++ bodyLocals)

-- =====================================================
-- Non-Tail (Assign)
-- =====================================================

explicateAssign :: String -> N1.Exp -> Tail -> Result (Tail, [String])
explicateAssign sym e cont =
  case e of
    N1.Int n ->
      Right (Seq (Assign sym (Atm (CPasses.C0.Int (fromIntegral n)))) cont, [sym])

    N1.Var x ->
      Right (Seq (Assign sym (Atm (CPasses.C0.Var x))) cont, [sym])

    N1.Read ->
      Right (Seq (Assign sym Read) cont, [sym])

    N1.Negate a -> do
      a' <- toAtom a
      Right (Seq (Assign sym (Sub a')) cont, [sym])

    N1.Add a b -> do
      a' <- toAtom a
      b' <- toAtom b
      Right (Seq (Assign sym (Add a' b')) cont, [sym])

    N1.Let x rhs body -> do
      (cont1, locals1) <- explicateAssign sym body cont
      (cont2, locals2) <- explicateAssign x rhs cont1
      Right (cont2, locals2 ++ locals1)