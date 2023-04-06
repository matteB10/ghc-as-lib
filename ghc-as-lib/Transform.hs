
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use lambda-case" #-}

module Transform where

import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt (..), valArgCount, CoreBind, AltCon (..), isRuntimeArg)
import GHC.Types.Name ( mkOccName, getOccString, mkInternalName, isDataConName, isSystemName)
import qualified GHC.Types.Name.Occurrence as Occ
--import qualified GHC.Types.Var as Var
import GHC.Types.Var
    ( Var(..),
      isTyVar,
      tyVarKind,
      isTcTyVar,
      isId,
      isTyCoVar,
      mkLocalVar,
      idInfo,
      idDetails,
      Id(..),
      isGlobalId,
      mustHaveLocalBinding,
      setVarType,
      mkGlobalVar,
      isLocalVar,
      setVarType, isTyVarBinder, setIdNotExported, mkExportedLocalVar, globaliseId )
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Core.Type (eqType, isPiTy, dropForAlls, isForAllTy_co, isForAllTy_ty, splitForAllTyCoVarBinders, splitForAllInvisTVBinders)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.Unique
import GHC.Core.TyCon (TyCon, mkPrelTyConRepName)

import GHC.Types.SrcLoc ( mkGeneralSrcSpan, srcLocSpan, GenLocated )
import GHC.Types.Id.Info (IdDetails(VanillaId), vanillaIdInfo, pprIdDetails)
import GHC.Plugins (IdEnv, getInScopeVars, showSDocUnsafe, Literal (LitString), mkDefaultCase, needsCaseBinding, mkLocalId, ModGuts (ModGuts, mg_binds), HscEnv (hsc_IC), InScopeSet, unsafeGetFreshLocalUnique, extendInScopeSet, mkInScopeSet, mkUniqSet, setIdExported, eltsUFM, getUniqSet, emptyInScopeSet, tryEtaReduce, isRuntimeVar, liftIO, manyDataConTy, multiplicityTy, splitForAllTyCoVar, showSDoc, floatBindings, FloatOutSwitches (..))
import GHC.Base ((<|>), Multiplicity)
import GHC.Data.Maybe (fromJust, liftMaybeT)
import GHC.Utils.Outputable (Outputable(ppr))
import GHC.Iface.Ext.Types (pprIdentifier)
import GHC.Tc.Utils.TcType (isTyConableTyVar)
import GHC.Core.Utils (exprType, exprIsExpandable, isExpandableApp)

import qualified Data.Text as T
import Control.Monad.Trans.State
import Data.Data (Data)
import Control.Monad (when, (>=>))
import Data.Maybe (isNothing, isJust)
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.Map (Map(..), insert, lookup)
import Data.List (intersect, delete, (\\))
import Control.Comonad.Identity (Identity(runIdentity), (<<=))
import Control.Lens (universeOf, universeOn)

import Data.Generics.Uniplate.Data
    ( rewriteBi, transformBi, transformBiM, universeBi, rewriteBiM, Biplate, universe )
import GHC.Types.Literal (Literal)
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Data.Bag (Bag)
import GHC.Parser.Annotation (SrcSpanAnnA)
import GHC.Hs (GhcTc, XUnboundVar)
import GHC.Hs.Binds (HsBindLR)
import GHC (HsExpr (..), Name, Ghc, HscEnv, GhcMonad (..), isPrimTyCon, getSessionDynFlags, DynFlags)
import GHC.Tc.Types.Evidence (HoleExprRef(..))
import GHC.Core.Lint (interactiveInScope)
import GHC.Runtime.Context (extendInteractiveContextWithIds)


import Utils ( isHole', isHoleExpr, isVarMod, varNameUnique )
import Similar ( Similar((~==)) )
import Data.Type.Equality (apply)
import Instance ( BiplateFor )
import qualified Data.Map as M
import Data.Generics.Uniplate (transform)
import GHC.Driver.Monad (modifySession)
import GHC.Core.Opt.Arity (etaExpandAT, exprArity, etaExpand, exprEtaExpandArity, arityTypeArity, findRhsArity)
import GHC.Core.Opt.Simplify.Utils (tryEtaExpandRhs)
import GHC.Types.Tickish (tickishCounts)
import Instance
import GHC.Core.Predicate (isEvVar)
import GHC.Core.Opt.FloatOut
import GHC.Utils.Logger (initLogger)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)


normalise :: String -> CoreProgram -> CoreProgram
-- | Combine normalising transformations
normalise funname = alpha funname . etaReduce . rewriteRec_ funname . repHoles_ . removeModInfo

normalise' :: String -> CoreProgram -> CoreProgram
-- | Normalise without renamíng 
normalise' funname =  etaReduce . removeModInfo

type Uq = (Char , Int)


data St = St {
         env  :: Map.Map Var Var
        ,freshUq   :: (Char,Int)
        ,freshNum  :: Int
        ,freshHNum :: Int
        ,exerName  :: String
        }


type Ctx a = State St a

-- \eta -> let f = ... in f eta 

freshVar :: Type -> Ctx Id
freshVar t = do
    (c,i) <- gets freshUq
    j <- gets freshNum
    let name = "fresh" ++ show j
    let id = mkVanillaVar name (c,i) t
    modify $ \s -> s {env = Map.insert id id (env s), freshNum = j+1, freshUq=(c,i+1)} -- update map 
    return id

mkVanillaVar :: String -> (Char,Int) -> Type -> Var
mkVanillaVar n uq t = mkLocalVar id_det name t t id_inf
        where id_det = VanillaId
              uq'    = uncurry mkUnique uq
              name   = mkInternalName uq' (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              id_inf = vanillaIdInfo

incUq :: (Char,Int) -> (Char,Int)
incUq (c,i) = (c, i+1)



newVar :: Id -> Ctx Id
newVar id = do
    j <- gets freshNum
    let name = "n_"++show j
    let id' = makeVar id name
    modify $ \s -> s {env = Map.insert id' id' (env s), freshNum = j+1} -- update map 
    return id'


makeVar' :: Unique -> Type -> String -> Id
makeVar' uq t n = mkLocalVar id_det name mult typ id_inf
        where id_det = VanillaId
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = multiplicityTy
              typ    = t
              id_inf = vanillaIdInfo


makeGlobVar :: Unique -> Type -> String -> Id
makeGlobVar uq t n = mkGlobalVar id_det name t id_inf
        where id_det = VanillaId
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              id_inf = vanillaIdInfo


makeVar :: Id -> String -> Id
makeVar id n = mkLocalVar id_det name mult typ id_inf
        where uq     = getUnique id
              id_det = idDetails id
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = varType id
              typ    = varType id
              id_inf = idInfo id

-- ========= ETA EXP =============

etaExpP :: CoreProgram -> Ghc CoreProgram
etaExpP p = do
    env <- getSession
    dflags <- getSessionDynFlags
    let ic = hsc_IC env
        inscopeVars = interactiveInScope ic
        is = mkInScopeSet $ mkUniqSet inscopeVars
    return $ goB dflags p
    where goB :: DynFlags -> CoreProgram -> CoreProgram
          goB df = transformBi $ \bi -> case bi :: CoreBind of
                b@(NonRec v e) | wantEtaExpB df v e -> NonRec v (eta e df)
                               | otherwise          -> goE df b
                b -> goE df b
          goE :: DynFlags -> CoreBind -> CoreBind
          goE df = transformBi $ \ex -> case ex :: CoreExpr of
            e        | wantEtaExpansion df e  -> eta e df
                     | otherwise -> e

          eta expr df = let arit = exprEtaExpandArity df expr -- in new version inscopeset is passed, but not retrieved.
                        in etaExpandAT arit expr             -- how can we update the inscope set with



-- Taken from GHC, not exported in 9.2.5
-- in previous version, exprIsExpandable would be the equivalent, but returns true in more cases. 
-- these cases are removed here since a lot of expansions would immediately get reduced again,
-- if also performing eta-reduction.
wantEtaExpansion :: DynFlags -> CoreExpr -> Bool
-- Mostly True; but False of PAPs which will immediately eta-reduce again
-- See Note [Which RHSs do we eta-expand?]
wantEtaExpansion df (Cast e _)             = wantEtaExpansion df e
wantEtaExpansion df (Tick _ e)             = wantEtaExpansion df e
wantEtaExpansion df (Lam b e) | isTyVar b  = wantEtaExpansion df e
wantEtaExpansion df (App e _)              = wantEtaExpansion df e
wantEtaExpansion _ (Var v)                 = False
wantEtaExpansion _ (Lit {})                = False
wantEtaExpansion df ex@(Let b e)           = exprArity ex < arityTypeArity id_arity
    where  id_arity = findRhsArity df (getBindTopVar b) ex (exprArity ex)
wantEtaExpansion _ _                        = True

wantEtaExpB :: DynFlags -> Var -> CoreExpr -> Bool
wantEtaExpB df v e = ex_arity < arityTypeArity id_arity
    where id_arity = findRhsArity df v e ex_arity
          ex_arity = exprArity e


-- ========= REPLACE HOLES =======
-- made monadic 

repHoles :: CoreProgram -> Ghc CoreProgram
repHoles prog = do
  env <- getSession
  let ic = hsc_IC env
  let inscopeVars = interactiveInScope ic
  (prog',inscopeVars') <- replaceHoles (mkInScopeSet $ mkUniqSet inscopeVars) prog
  let ic' = extendInteractiveContextWithIds ic (eltsUFM $ getUniqSet $ getInScopeVars inscopeVars')
  let env' = env {hsc_IC = ic'}
  modifySession $ const env'
  return prog'
        where replaceHoles :: InScopeSet -> CoreProgram -> Ghc (CoreProgram,InScopeSet)
              -- | Replace expressions representing holes with hole variables of the same type 
              replaceHoles is cs = runStateT (tr cs) is
                where tr :: CoreProgram -> StateT InScopeSet Ghc CoreProgram
                      tr = transformBiM $ \case
                        c@(Case e v t _) | isHoleExpr c ->
                                               let id = fromJust (getTypErr e)
                                               in newHoleVar t >>= \id -> return $ Var id
                                         | otherwise -> return c
                        e -> return e

newHoleVar :: Type -> StateT InScopeSet Ghc Var
newHoleVar t = do
    is <- get
    let uq = unsafeGetFreshLocalUnique is
    let is' = case t of
          (TyVarTy tv) -> extendInScopeSet is' tv
          typ          -> is
    let name = "hole"
        id   = setIdExported $ setVarType (makeGlobVar uq t name) t
        is'' = extendInScopeSet is' id
    put is''
    return id

--- old non-ghc version 
repHoles_ :: CoreProgram -> CoreProgram
repHoles_ prog = evalState (go prog) initSt
        where go :: CoreProgram -> Ctx CoreProgram
              go = transformBiM $ \case
                        c@(Case e v t _) | isHoleExpr c ->
                                                           let id = fromJust (getTypErr e)
                                                           in newHoleVar_ t >>= \id -> return $ Var id
                                         | otherwise -> return c
                        e -> return e

newHoleVar_ :: Type -> Ctx Id
newHoleVar_ t = do
    uq <- gets freshUq
    let name = "hole"
        id   = setIdExported $ setVarType (makeGlobVar (uncurry mkUnique uq) t name) t
    return id


getTypErr :: Expr Var -> Maybe Var
getTypErr e = head [Just v | (Var v) <- universe e, getOccString v == "typeError"]


alphaWCtxt :: String -> CoreProgram -> (CoreProgram, Map.Map Var Var)
-- | Do renaming and return map of renamed variables        
alphaWCtxt fname cs = (evalState st initialState, map)
    where st@(StateT f) = mapM alphaR cs
          map = env $ execState st initialState
          initialState = (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fname})

alpha :: String -> CoreProgram -> CoreProgram
-- | Rename all local non-type variables, replace holes with variable s
alpha fname cs = evalState cs' (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fname})
    where cs' = mapM alphaR cs


alphaR :: Bind Var -> Ctx (Bind Var)
alphaR (NonRec v e) = do
        v' <- renameVar v
        NonRec v' <$> aRename e
alphaR (Rec es) = do
        vars <- mapM (renameVar . fst) es
        exps <- mapM (aRename . snd) es
        return $ Rec (zip vars exps)

renameVar :: Id -> Ctx Id
renameVar v = --trace ("var:" ++ show v ++ " is tyvar: " ++ show (isTyVar v) ++ " musthavebind: " ++ show (mustHaveLocalBinding v) ++ " islocal: " ++ show (isLocalVar v) ++ " isSysName "++show (isSystemName (varName v))) 
            do
            env <- gets env
            name <- gets exerName
            case Map.lookup v env of
                Just n  -> return n
                Nothing -> checkNew v name
    where checkNew v n | varNameUnique v == n = return v -- dont rename main function
                       | isGlobalId v = return v
                       | isTyVar v    = return v
                       | isTyCoVar v  = return v   ------ this is a problem in the simplifier pass, since local helper gets global ids
                                                   ------ but we dont want to rename global id's like (:)
                       -- | isPrimTyCon $ mkPrelTyConRepName (varName v) = return v 
                       | take 1 (getOccString v) == "$" = return v  -- ugly hack for now 
                       | otherwise    = newVar v   ------ need to check for type constraints as $EqInteger 


aRename :: Expr Var -> Ctx (Expr Var)
aRename v@(Var id)     | isHole' v = return v
                       | otherwise  = Var <$> renameVar id
aRename t@(Type _)     = return t
aRename l@(Lit _)      = return l
aRename (App e arg)    = do
        e' <- aRename e
        arg' <- aRename arg
        return $ App e' arg'
aRename l@(Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar b
        e' <- aRename e
        return $ Lam b' e'
aRename c@(Case e v t a) = do
                            e' <- aRename e
                            v' <- renameVar v
                            a' <- renameAlt a
                            return $ Case e' v' t a'
aRename (Cast e co)    = trace ("DO we ever find coercion " ++ show co) $
                         do
        e' <- aRename e
        return $ Cast e' co
aRename (Let b e)      = do
    b' <- alphaR b
    e' <- aRename e
    return $ Let b' e'
aRename (Tick ct e)    = aRename e >>= \e' -> return (Tick ct e')
aRename x              = return x

renameAlt :: [Alt Var] -> Ctx [Alt Var]
renameAlt = mapM renameAlt'
    where renameAlt' :: Alt Var -> Ctx (Alt Var)
          renameAlt' (Alt ac vs e) = do
            vs' <- mapM renameVar vs
            e' <- aRename e
            return $ Alt ac vs' e'

getTypErrB :: CoreBind -> Maybe Id
getTypErrB (NonRec _ e) = getTypErr e
getTypErrB (Rec es) = case filter isNothing k of
                    [] -> Nothing
                    l  -> Just (fromJust $ head l)
        where k = map (getTypErr . snd) es



removeModInfo :: CoreProgram -> CoreProgram
removeModInfo = concatMap removeModInf
    where removeModInf :: Bind Var -> [Bind Var]
          removeModInf (NonRec v e) | isVarMod v = []
          removeModInf b                         = [b]


etaReduce :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
etaReduce = rewriteBi etaRed


etaRed :: Expr Var ->  Maybe (Expr Var)
-- | eta reduction, e.g., \x -> f x => f
etaRed (Lam v (App f args)) =
   case args of
      Var v' | v == v' -> return f
      _                -> Nothing
etaRed _ = Nothing

etaReduceTy :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
etaReduceTy = rewriteBi etaRedTy


etaRedTy :: Expr Var -> Maybe (Expr Var)
-- | type 
etaRedTy (App f (Type (TyVarTy v))) | isTyVar v = return f 
                                    | otherwise     = Nothing
etaRedTy _ = Nothing



rewriteBinds :: String -> CoreProgram -> Ghc CoreProgram
-- | Inline recursive binders as let-recs when appropriate
rewriteBinds fn prog = do
  env <- getSession
  let ic = hsc_IC env
  let inscopeVars = interactiveInScope ic
      is = mkInScopeSet $ mkUniqSet inscopeVars
  (prog',is') <- runStateT (recToNonRec is prog) is
  (prog'',is'') <- runStateT (inlineRec is' prog') is' 
  let ic' = extendInteractiveContextWithIds ic (eltsUFM $ getUniqSet $ getInScopeVars is'')
  let env' = env {hsc_IC = ic'}
  setSession env'
  return prog''


recToNonRec :: InScopeSet -> CoreProgram -> StateT InScopeSet Ghc CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder
recToNonRec is [] = return []
recToNonRec is (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do -- RECS WITH MORE ITEMS NOT HANDLED 
                if isForAllTy_ty (varType v) then do -- this is tr
                    let (_,ft) = splitForAllInvisTVBinders (varType v) -- remove forall type (if not explicitly typed) from (soon) inlined function
                    fresh <- freshGhcVar ft -- constraints should also be dropped => requires change in expression  
                    let e' = subst fresh v e
                        is' = extendInScopeSet is fresh
                    put is'
                    --liftIO $ putStrLn $ "Rec " ++ show v ++ " typ: " ++ showSDocUnsafe (ppr ft)
                    recToNonRec is bs >>= \bs' -> return $ NonRec v (liftTypConstraints (Let (Rec ((fresh,e'):ls)) (Var fresh))):bs
                    --return $ NonRec v (Let (Rec ((fresh,e):ls)) (Var fresh)):bs' -- rewrite as a nonrec
                else do
                    fresh <- freshGhcVar (varType v)
                    let e' = subst fresh v e
                        is' = extendInScopeSet is fresh
                    put is'
                    recToNonRec is bs >>= \bs' -> return $ NonRec v (Let (Rec ((fresh,e'):ls)) (Var fresh)):bs
    nr@(NonRec v e) -> recToNonRec is bs >>= \bs' -> return $ nr : bs'

inlineRec :: InScopeSet -> CoreProgram -> StateT InScopeSet Ghc CoreProgram
inlineRec is [] = return []
inlineRec is (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> (liftIO $ putStrLn "should not have any top level recs left") >> 
                                                    inlineRec is bs >>= \bs' -> return $ rr:bs' 
    nr@(NonRec v e) -> do
         if any (insB v) bs then do 
                                let (newBinds, rest) = inline nr bs in inlineRec is rest >>= \bs' -> return $ newBinds ++ bs'
                            else inlineRec is bs >>= \bs' -> return $ nr:bs'
   {-  nr@(NonRec v e) | any (insB v) bs -> let (newBinds, rest) = inline nr bs
                                         in inlineRec is rest >>= \bs' -> return $ newBinds ++ bs'
                    | otherwise -> inlineRec is bs >>= \bs' -> return $ nr:bs' -}

rewriteRecGhc :: String -> CoreProgram -> Ghc CoreProgram
-- | Inline recursive binders as let-recs when appropriate
rewriteRecGhc fn prog = do
  env <- getSession
  let ic = hsc_IC env
  let inscopeVars = interactiveInScope ic
      is = mkInScopeSet $ mkUniqSet inscopeVars
  (prog',inscopeVars') <- runStateT (rewriteRecs is prog) is
  let ic' = extendInteractiveContextWithIds ic (eltsUFM $ getUniqSet $ getInScopeVars inscopeVars')
  let env' = env {hsc_IC = ic'}
  setSession env'
  return prog'

rewriteRecs :: InScopeSet -> CoreProgram -> StateT InScopeSet Ghc CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder (if not used somewhere in the program)
--   Inline Recs as Let-Recs into NonRecs 
rewriteRecs is [] = return []
rewriteRecs is (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there
                                 --  might be bad for performanc
            then
                let (newBinds, rest) = inline rr bs
                in rewriteRecs is rest >>= \bs' -> return $ newBinds ++ bs'
            else do -- if top-level rec not used somewhere else, insert it in a toplevel non-rec
                if isForAllTy_ty (varType v) then do
                    let (_,ft) = splitForAllTyCoVarBinders (varType v) -- remove forall from (soon) inlined function
                    fresh <- freshGhcVar ft -- constraints should also be dropped => requires change in expression  
                    let e' = subst fresh v e
                        is' = extendInScopeSet is fresh
                    put is'
                    liftIO $ putStrLn $ "Rec " ++ show v ++ " typ: " ++ showSDocUnsafe (ppr ft)
                    rewriteRecs is bs >>= \bs' -> return $ NonRec v (liftTypConstraints (Let (Rec ((fresh,e'):ls)) (Var fresh))):bs
                    --return $ NonRec v (Let (Rec ((fresh,e):ls)) (Var fresh)):bs' -- rewrite as a nonrec
                else do
                    fresh <- freshGhcVar (varType v)
                    let e' = subst fresh v e
                        is' = extendInScopeSet is fresh
                    put is'
                    rewriteRecs is bs >>= \bs' -> return $ NonRec v (Let (Rec ((fresh,e'):ls)) (Var fresh)):bs

    nr@(NonRec v e) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there

            then
                let (newBinds, rest) = inline nr bs
                in rewriteRecs is rest >>= \bs' -> return $ newBinds ++ bs'
            else
                rewriteRecs is bs >>= \bs' -> return $ nr:bs'

dropConstr :: Type -> Type
-- | Drop outer type constraints when inlining. Not sure if this generally holds 
dropConstr f@(FunTy {}) = ft_res (ft_res f)
dropConstr t = t

--isEvVar (Var ($dEq)) == True 
--isEvVar (Var $fEq[]) == True

liftTypConstraints :: Expr Var -> Expr Var
liftTypConstraints (Let (Rec ((b,Lam v e):ls)) ine) | isEvVar v || isTyVar v = Lam v $ liftTypConstraints (Let (Rec ((b,e):ls)) ine)
liftTypConstraints e = e


freshGhcVar :: Type -> StateT InScopeSet Ghc Id
freshGhcVar t = do
    is <- get
    let uq = unsafeGetFreshLocalUnique is
    let name = "fresh"
        id'  = setIdNotExported $ makeVar' uq t name
        is'  = extendInScopeSet is id'
    put is'
    return id'

-- Non-ghc version

recToNonRecs_ :: String -> CoreProgram -> CoreProgram
recToNonRecs_ fname cs = evalState cs' (initSt {exerName = fname})
    where cs' = recToNonRec_ cs

recToNonRec_ :: CoreProgram -> Ctx CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder
recToNonRec_ [] = return []
recToNonRec_ (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do  
                if isForAllTy_ty (varType v) then do -- this is tr
                    let (_,ft) = splitForAllInvisTVBinders (varType v) -- remove forall type (if not explicitly typed) from (soon) inlined function
                    fresh <- freshVar ft -- constraints should also be dropped => requires change in expression  
                    let e' = subst fresh v e
                    --liftIO $ putStrLn $ "Rec " ++ show v ++ " typ: " ++ showSDocUnsafe (ppr ft)
                    recToNonRec_ bs >>= \bs' -> return $ NonRec v (liftTypConstraints (Let (Rec ((fresh,e'):ls)) (Var fresh))):bs
                    --return $ NonRec v (Let (Rec ((fresh,e):ls)) (Var fresh)):bs' -- rewrite as a nonrec
                else do
                    fresh <- freshVar (varType v)
                    let e' = subst fresh v e
                    recToNonRec_ bs >>= \bs' -> return $ NonRec v (Let (Rec ((fresh,e'):ls)) (Var fresh)):bs
    nr@(NonRec v e) ->
                recToNonRec_ bs >>= \bs' -> return $ nr : bs'
    _ -> trace ("do we get here") return []

rewriteRec_ :: String -> CoreProgram -> CoreProgram
rewriteRec_ fname cs = evalState cs' (initSt {exerName = fname})
    where cs' = rewriteRecs_ cs

rewriteRecs_ :: CoreProgram -> Ctx CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder (if not used somewhere in the program)
--   Inline Recs as Let-Recs into NonRecs 
rewriteRecs_ [] = return []
rewriteRecs_ (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there
                                 --  might be bad for performanc
            then
                let (newBinds, rest) = inline rr bs
                in rewriteRecs_ rest >>= \bs' -> return $ newBinds ++ bs'
            else if isPiTy (varType v) then do
                    fresh <- freshVar (varType v) -- dropForAlls ? 
                    e' <- subst_ fresh v e
                    trace ("replacing " ++ show v ++ " e: " ++ show e')
                        rewriteRecs_ bs >>= \bs' -> return $ NonRec v (liftTypConstraints (Let (Rec ((fresh, e'):ls)) (Var fresh))):bs
                 else do
                    fresh <- freshVar (varType v)
                    e' <- subst_ fresh v e
                    rewriteRecs_ bs >>= \bs' -> return $ NonRec v (Let (Rec ((fresh,e'):ls)) (Var fresh)):bs

    nr@(NonRec v e) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there
                             --  might be bad for performance
            then
                let (newBinds, rest) = inline nr bs
                in rewriteRecs_ rest >>= \bs' -> return $ newBinds ++ bs'
            else
                rewriteRecs_ bs >>= \bs' -> return $ nr:bs'

subst :: Var -> Var -> CoreExpr -> CoreExpr
subst v v' = --trace ("found subst" ++ show "["++ show v' ++ "->" ++ show v ++"]" ) $
             transformBi (sub v v')

sub :: Var -> Var -> CoreExpr -> CoreExpr
-- | Replace the second variable with the first one given
sub v v' = \case
    (Var id) | id == v' -> (Var v)
    e -> e

ins :: Data (Expr Var) => Var -> Expr Var -> Bool
-- | Find if a variable is used somewhere in an expr
ins n e = or [v==n | v <- universeBi e :: [Var]]

insB :: Data (Bind Var) => Var -> Bind Var -> Bool
-- | Find if a variable is used somewhere in a binder
insB n b = or [v==n | v <- universeBi b :: [Var]]


inline :: Bind Var -> [Bind Var] -> ([Bind Var],[Bind Var])
-- | Inline a binder and return remaining binders 
inline b bs = let ls  = getBind bs (getBindTopVar b) -- get all binders using the binder we want to inline 
                  b'  = setBindTopVar (makeLocal (getBindTopVar b)) b -- change scope to local of binder variable if inlined 
                  bs' = insertBind b ls
                  in (bs', delete b (bs \\ ls))

makeLocal :: Var -> Var
makeLocal v | isId v  && isGlobalId v = mkLocalId (varName v) (varMult v) (varType v)
              | isId v                  = v
              | otherwise = v


setBindTopVar :: Var -> Bind Var -> Bind Var
setBindTopVar v = transformBi $ \e -> case e :: CoreExpr of
        (Var v') | v == v' -> Var v
        e       -> e

getBindTopVar :: Bind Var -> Var
-- | Get variable of a binder 
getBindTopVar (NonRec v _) = v
getBindTopVar (Rec ((v,e):_)) = v


getBind :: [CoreBind] -> Var -> [Bind Var]
-- | Get all binders containing a certain variable
getBind binds v = [r | r <- binds, v `insB` r]


bindNames :: Bind Var -> [Name]
-- | Return all names used in a binder 
bindNames = \case
    Rec ((v,e):es) -> varName v:bindNamesE e
    NonRec v e     -> varName v:bindNamesE e
    where bindNamesE :: Expr Var -> [Name]
          bindNamesE e = concat [bindNames n | Let n e' <- universeBi e]


insertBind :: Bind Var -> [Bind Var] -> [Bind Var]
-- | Inline binder in all binders using it 
insertBind n@(NonRec v e) bs = map insertB bs -- inline another nonrec 
    where insertB bi@(NonRec b e') = (transformBi $ \case
            (Var v') | v == v' -> e
            e -> e) bi
insertBind (Rec _) _ = error "All top-level binders should be NonRecs"


floatOut :: CoreProgram -> Ghc CoreProgram 
floatOut p = do 
    df <- getSessionDynFlags
    logger <- liftIO initLogger 
    let floatSw = FloatOutSwitches {
            floatOutLambdas = Nothing,    -- float all lambdas to top level,
            floatOutConstants = False,    -- True => float constants to top level,
            floatOutOverSatApps = False,  -- True => float out over-saturated application
            floatToTopLevelOnly = False   -- Allow floating to the top level only.
            } 
    us <- liftIO $ mkSplitUniqSupply 'z'
    liftIO $ floatOutwards logger floatSw df us p 




--- EXPERIMENTAL STUFF BELOW
----------------------------------------------------

addDefaultCase :: CoreProgram -> CoreProgram
addDefaultCase p = evalState (pm p) initSt
    where pm :: CoreProgram -> Ctx CoreProgram
          pm = transformBiM addDefCase

initSt :: St
initSt = St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = " ", freshUq = ('x',1)}

addDefCase :: Expr Var -> Ctx (Expr Var)
addDefCase ex@(Lam v e) | isTyVar v                     = Lam v <$> addDefCase e
                        | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                           -- needs to use a case rather than let bind
addDefCase e = return e


addCase :: Expr Var -> Var -> Ctx (Expr Var)
addCase e v = do
    let t = varType v
    fresh <- freshVar t
    wild <- freshVar t  -- should have same type as the case 
    e' <- subst_ fresh v e
    return $ Lam fresh $ mkDefaultCase (Var fresh) wild e'

subst_ :: Var -> Var -> CoreExpr -> Ctx CoreExpr
subst_ v v' = --trace ("found subst" ++ show "["++ show v' ++ "->" ++ show v ++"]" ) $
             transformBiM (sub_ v v')

sub_ :: Var -> Var -> CoreExpr -> Ctx CoreExpr
-- | Replace all occurences of the second variable with the first one in the given expr
sub_ v v' = \case
    (Var id) | id == v' -> do
        modify $ \s -> s {env = Map.insert v v' (env s)}
        return (Var v)
    e -> return e







-- Core Utils 
-- mkSingleAltCase
-- needsCaseBInding
-- bindNonRec
-- mkAltExpr -- make case alternatives 
-- | Extract the default case alternative
-- findDefault :: [Alt b] -> ([Alt b], Maybe (Expr b))
-- -- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
-- findAlt :: AltCon -> [Alt b] -> Maybe (Alt b)

-- check if we can use diffBinds from Core.Utils to find small diffs 
-- diffExpr instead of my similarity relation? need an RnEnv2

-- mkLamTypes :: [Var] -> Type -> Type
-- can this be used for beta-expansioN???????

-- applyTypeToArgs :: HasDebugCallStack => SDoc -> Type -> [CoreExpr] -> Type
-- ^ Determines the type resulting from applying an expression with given type
--- to given argument expressions.
-- Do I need to do this backwards when eta-reducing?

caseToGuard :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
caseToGuard = rewriteBi etaRed


ctg :: Expr Var ->  Maybe (Expr Var)
-- | case to guard, e.g. case e of {Just a -> a} => case (e == Just a) of {True -> a}
ctg (Lam v (App f args)) =
   case args of
      Var v' | v == v' -> return f
      _                -> Nothing
ctg _ = Nothing