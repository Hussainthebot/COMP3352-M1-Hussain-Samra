module N1Passes where
import N1
import Env
import CompilerPasses

-- =====================================================
-- SHARED HELPERS
-- =====================================================

getResult :: CompilerResult s Program -> Result Program
getResult (CState _ (Right p)) = Right p
getResult (CState _ err)       = err

-- =====================================================
-- PASS 1: UNIQUIFY
-- =====================================================

data UniquifyState = UState (Env String) Integer

type UniquifyResult = CompilerResult UniquifyState Exp

uniquify :: Program -> CompilerResult UniquifyState Program
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state $ Right $ Program exp'
    CState state (Left msg)   -> CState state $ Left msg

uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

uniquifyExp v@(Int _) state = CState state $ Right v
uniquifyExp r@Read state = CState state $ Right r

uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' $ Right $ Negate res
    err                       -> err

uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state' (Right x') ->
      case uniquifyExp y state' of
        CState state'' (Right y') -> CState state'' $ Right $ Add x' y'
        err -> err
    err -> err

uniquifyExp (Var sym) state@(UState env _) =
  case Env.lookupEnv sym env of
    Just sym' -> CState state $ Right $ Var sym'
    Nothing   -> CState state $ Left $ "Symbol '" ++ sym ++ "' not found"

uniquifyExp (Let sym expr body) (UState env counter) =
  let freshSym  = "s" ++ show counter
      newState  = UState env (counter + 1)
  in
  case uniquifyExp expr newState of
    CState (UState _ counter') (Right expr') ->
      let extEnv   = Env.extendEnv sym freshSym env
          bodyState = UState extEnv counter'
      in
      case uniquifyExp body bodyState of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let freshSym expr' body'
        err -> err
    CState state' (Left msg) -> CState state' $ Left msg

-- =====================================================
-- PASS 2: REMOVE COMPLEX OPERA*
-- =====================================================

type RCOState  = Integer
type RCOResult = CompilerResult RCOState Exp

type AtmResult = CompilerResult RCOState (Exp, [(String, Exp)])

passRemoveComplexOperas :: CompilerResult RCOState Program -> CompilerResult RCOState Program
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right exp') -> CState symCount' $ Right $ Program exp'
    CState symCount' (Left msg)   -> CState symCount' $ Left msg
passRemoveComplexOperas err@(CState _ (Left _)) = err

rcoExp :: RCOResult -> RCOResult

rcoExp atm@(CState _ (Right Read))      = atm
rcoExp atm@(CState _ (Right (Int _)))   = atm
rcoExp atm@(CState _ (Right (Var _)))   = atm

rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState state' (Right expr') ->
      case rcoExp (CState state' (Right body)) of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let sym expr' body'
        err -> err
    err -> err

rcoExp (CState state (Right (Negate expr))) =
  case rcoAtm (CState state (Right (expr, []))) of
    CState state' (Right (atom, bindings)) ->
      CState state' $ Right $ wrapLets bindings (Negate atom)
    CState state' (Left msg) -> CState state' $ Left msg

rcoExp (CState state (Right (Add x y))) =
  case rcoAtm (CState state (Right (x, []))) of
    CState state' (Right (atomX, bindingsX)) ->
      case rcoAtm (CState state' (Right (y, []))) of
        CState state'' (Right (atomY, bindingsY)) ->
          CState state'' $ Right $ wrapLets (bindingsX ++ bindingsY) (Add atomX atomY)
        CState state'' (Left msg) -> CState state'' $ Left msg
    CState state' (Left msg) -> CState state' $ Left msg

rcoExp (CState _ (Left msg)) = CState 0 (Left msg)

rcoAtm :: AtmResult -> AtmResult

rcoAtm res@(CState _ (Right (Int _, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res
rcoAtm res@(CState _ (Right (Read, _)))  = res

rcoAtm (CState symCount (Right (Negate expr, lst))) =
  case rcoAtm (CState symCount (Right (expr, []))) of
    CState symCount' (Right (innerAtom, innerBindings)) ->
      let freshSym = "s" ++ show symCount'
          negExpr  = Negate innerAtom
          allBindings = lst ++ innerBindings ++ [(freshSym, negExpr)]
      in CState (symCount' + 1) $ Right (Var freshSym, allBindings)
    err -> err

rcoAtm (CState symCount (Right (Add e1 e2, lst))) =
  case rcoAtm (CState symCount (Right (e1, []))) of
    CState symCount' (Right (atom1, bindings1)) ->
      case rcoAtm (CState symCount' (Right (e2, []))) of
        CState symCount'' (Right (atom2, bindings2)) ->
          let freshSym = "s" ++ show symCount''
              addExpr = Add atom1 atom2
              allBindings = lst ++ bindings1 ++ bindings2 ++ [(freshSym, addExpr)]
          in CState (symCount'' + 1) $ Right (Var freshSym, allBindings)
        err -> err
    err -> err

rcoAtm (CState symCount (Right (Let sym expr body, lst))) =
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right expr') ->
      case rcoAtm (CState symCount' (Right (body, []))) of
        CState symCount'' (Right (bodyAtom, bodyBindings)) ->
          let innerExp = wrapLets bodyBindings bodyAtom
              letExpr = Let sym expr' innerExp
              freshSym = "s" ++ show symCount''
              allBindings = lst ++ [(freshSym, letExpr)]
          in CState (symCount'' + 1) $ Right (Var freshSym, allBindings)
        CState st (Left msg) -> CState st (Left msg)
    CState st (Left msg) -> CState st (Left msg)

rcoAtm (CState state (Left msg)) = CState state (Left msg)

wrapLets :: [(String, Exp)] -> Exp -> Exp
wrapLets [] body = body
wrapLets ((sym,e):rest) body = Let sym e (wrapLets rest body)
