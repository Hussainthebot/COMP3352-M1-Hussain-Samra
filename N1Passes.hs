module N1Passes where

import N1
import Env
import CompilerPasses

-- =====================================================
-- SHARED HELPERS
-- =====================================================

-- | Pull the final Result out of a uniquify CompilerResult
getResult :: CompilerResult UniquifyState Program -> Result Program
getResult (CState _ (Right p)) = Right p
getResult (CState _ err)       = err

-- =====================================================
-- PASS 1: UNIQUIFY
-- =====================================================
-- Every let-bound variable is renamed to a globally
-- fresh symbol "sN".  References are updated via the
-- environment.  A reference to an unbound variable is
-- a compile-time error.
-- =====================================================

data UniquifyState = UState (Env String) Integer

type UniquifyResult = CompilerResult UniquifyState Exp

-- | Top-level uniquify entry point
uniquify :: Program -> CompilerResult UniquifyState Program
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state $ Right $ Program exp'
    CState state (Left msg)   -> CState state $ Left msg

-- | Recursively rename variables inside an expression.
--   The UniquifyState carries the current renaming environment
--   and a counter used to generate fresh names.
uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

-- Integers are atomic; pass through unchanged
uniquifyExp v@(Int _) state = CState state $ Right v

-- Read is atomic; pass through unchanged
uniquifyExp r@Read state = CState state $ Right r

-- Negate: uniquify the sub-expression, then rebuild
uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' $ Right $ Negate res
    err                       -> err

-- Add: uniquify the left operand, then the right using the
-- updated state from the left.
uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state' (Right x') ->
      case uniquifyExp y state' of
        CState state'' (Right y') -> CState state'' $ Right $ Add x' y'
        err                       -> err
    err -> err

-- Var: look up the current unique name in the environment.
--      If not found, report an error.
uniquifyExp (Var sym) state@(UState env _) =
  case Env.lookupEnv sym env of
    Just sym' -> CState state $ Right $ Var sym'
    Nothing   -> CState state $ Left $ "Symbol '" ++ sym ++ "' not found"

-- Let: generate a fresh name for the bound variable, extend
--      the environment, uniquify the binding expression under
--      the *old* environment (the bound name is not in scope
--      for its own definition), then uniquify the body under
--      the new environment.
uniquifyExp (Let sym expr body) (UState env counter) =
  let freshSym  = "s" ++ show counter
      newState  = UState env (counter + 1)          -- counter bumped; env unchanged for expr
  in
  case uniquifyExp expr newState of
    CState (UState _ counter') (Right expr') ->
      let extEnv   = Env.extendEnv sym freshSym env  -- extend ORIGINAL env with fresh name
          bodyState = UState extEnv counter'
      in
      case uniquifyExp body bodyState of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let freshSym expr' body'
        err -> err
    CState state' (Left msg) -> CState state' $ Left msg

-- =====================================================
-- PASS 2: REMOVE COMPLEX OPERA* (RCO / ANF)
-- =====================================================
-- After this pass every argument to Add and Negate is
-- atomic (an Int or a Var).  Non-atomic sub-expressions
-- are lifted into fresh let-bindings.
--
-- atm  ::= Int n  |  Var x
-- exp  ::= atm | Read | Negate atm | Add atm atm
--        | Let x exp exp
-- =====================================================

type RCOState  = Integer
type RCOResult = CompilerResult RCOState Exp

-- | A result carrying (the atom that replaces this sub-expression,
--   the accumulated list of (freshName, complexExpr) bindings that
--   must be wrapped around the use-site as nested Lets).
type AtmResult = CompilerResult RCOState (Exp, [(String, Exp)])

-- | Top-level entry point for the RCO pass.
--   Accepts a CompilerResult so the counter is threaded naturally.
passRemoveComplexOperas :: CompilerResult RCOState Program -> CompilerResult RCOState Program
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right exp') -> CState symCount' $ Right $ Program exp'
    CState symCount' (Left msg)   -> CState symCount' $ Left msg
passRemoveComplexOperas err@(CState _ (Left _)) = err

-- | Walk an expression that may be atomic *or* complex.
--   Returns the expression in ANF form.
rcoExp :: RCOResult -> RCOResult

-- Atomic base cases: unchanged
rcoExp atm@(CState _ (Right Read))      = atm
rcoExp atm@(CState _ (Right (Int _)))   = atm
rcoExp atm@(CState _ (Right (Var _)))   = atm

-- Let: RCO the binding expression and the body independently,
--      threading the counter.  Let itself can contain complex
--      sub-expressions in its binding position.
rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState state' (Right expr') ->
      case rcoExp (CState state' (Right body)) of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let sym expr' body'
        err -> err
    err -> err

-- Negate: the argument must become atomic.
--   Use rcoAtm to get an atom + accumulated bindings, then
--   wrap the rebuilt Negate in those Let bindings.
rcoExp (CState state (Right (Negate expr))) =
  case rcoAtm (CState state (Right (expr, []))) of
    CState state' (Right (atom, bindings)) ->
      CState state' $ Right $ wrapLets bindings (Negate atom)
    CState state' (Left msg) -> CState state' $ Left msg

-- Add: both operands must become atomic.
--   Atomise the left first (collecting its bindings), then
--   atomise the right (collecting *its* bindings), finally
--   combine all bindings and rebuild Add.
rcoExp (CState state (Right (Add x y))) =
  case rcoAtm (CState state (Right (x, []))) of
    CState state' (Right (atomX, bindingsX)) ->
      case rcoAtm (CState state' (Right (y, []))) of
        CState state'' (Right (atomY, bindingsY)) ->
          CState state'' $ Right $ wrapLets (bindingsX ++ bindingsY) (Add atomX atomY)
        CState state'' (Left msg) -> CState state'' $ Left msg
    CState state' (Left msg) -> CState state' $ Left msg

-- Propagate errors
rcoExp (CState _ (Left msg)) = CState 0 (Left msg)

-- | Attempt to reduce an expression to an atomic form.
--   Returns the atom to use at the call-site *and* a list of
--   (name, expr) pairs that must be introduced as Let bindings
--   around the call-site (outermost last = innermost first).
rcoAtm :: AtmResult -> AtmResult

-- Already-atomic cases: nothing to do
rcoAtm res@(CState _ (Right (Int _,  _))) = res
rcoAtm res@(CState _ (Right (Var _,  _))) = res
rcoAtm res@(CState _ (Right (Read,   _))) = res

-- Negate: recursively atomise its argument, then lift the
--         whole Negate into a fresh binding.
rcoAtm (CState symCount (Right (Negate expr, lst))) =
  -- First, atomise the inner expression
  case rcoAtm (CState symCount (Right (expr, []))) of
    CState symCount' (Right (innerAtom, innerBindings)) ->
      let freshSym  = "s" ++ show symCount'
          negExpr   = Negate innerAtom
          -- append inner bindings, then this Negate binding
          allBindings = lst ++ innerBindings ++ [(freshSym, negExpr)]
      in CState (symCount' + 1) $ Right (Var freshSym, allBindings)
    err -> err

-- Add: atomise both sub-expressions, lift the whole Add
--      into a fresh binding.
rcoAtm (CState symCount (Right (Add e1 e2, lst))) =
  case rcoAtm (CState symCount (Right (e1, []))) of
    CState symCount' (Right (atom1, bindings1)) ->
      case rcoAtm (CState symCount' (Right (e2, []))) of
        CState symCount'' (Right (atom2, bindings2)) ->
          let freshSym    = "s" ++ show symCount''
              addExpr     = Add atom1 atom2
              allBindings = lst ++ bindings1 ++ bindings2 ++ [(freshSym, addExpr)]
          in CState (symCount'' + 1) $ Right (Var freshSym, allBindings)
        err -> err
    err -> err

-- Let: atomise the body (the value produced by the let),
--      keeping the Let as a compound binding.
rcoAtm (CState symCount (Right (Let sym expr body, lst))) =
  -- RCO the expr and body expressions first
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right expr') ->
      case rcoAtm (CState symCount' (Right (body, []))) of
        CState symCount'' (Right (bodyAtom, bodyBindings)) ->
          -- Build: Let sym expr' (... bodyBindings wrapped ... bodyAtom)
          let innerExp    = wrapLets bodyBindings bodyAtom
              letExpr     = Let sym expr' innerExp
              freshSym    = "s" ++ show symCount''
              allBindings = lst ++ [(freshSym, letExpr)]
          in CState (symCount'' + 1) $ Right (Var freshSym, allBindings)
        err -> err
    err -> err

-- Propagate errors
rcoAtm (CState _ (Left msg)) = CState 0 (Left msg, [])

-- =====================================================
-- INTERNAL HELPER
-- =====================================================

-- | Given a list of (name, expr) bindings accumulated during
--   rcoAtm and a final body expression, build nested Let nodes.
--   The list is ordered outermost-first, matching the order
--   returned by rcoAtm.
wrapLets :: [(String, Exp)] -> Exp -> Exp
wrapLets []                body = body
wrapLets ((sym, e) : rest) body = Let sym e (wrapLets rest body)