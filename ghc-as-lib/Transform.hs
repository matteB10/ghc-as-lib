
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use lambda-case" #-}

module Transform where

import GHC (Name, Ghc, HscEnv, GhcMonad (..), getSessionDynFlags, isLocalId, SrcSpan)
import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt (..), valArgCount, CoreBind, AltCon (..))
import GHC.Types.Name ( mkOccName, getOccString, mkInternalName, isDataConName, isSystemName, getSrcSpan)
import qualified GHC.Types.Name.Occurrence as Occ
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
      setVarType, isTyVarBinder, setIdNotExported, mkExportedLocalVar, globaliseId, setVarName )
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.Unique
import GHC.Core.TyCon (TyCon, mkPrelTyConRepName)

import GHC.Types.SrcLoc ( mkGeneralSrcSpan, srcLocSpan, GenLocated )
import GHC.Types.Id.Info (IdDetails(VanillaId), vanillaIdInfo, pprIdDetails, setOccInfo, IdInfo, setArityInfo, arityInfo)
import GHC.Plugins (IdEnv, getInScopeVars, showSDocUnsafe, Literal (LitString), mkDefaultCase, needsCaseBinding, mkLocalId, ModGuts (ModGuts, mg_binds), HscEnv (hsc_IC), InScopeSet, unsafeGetFreshLocalUnique, extendInScopeSet, mkInScopeSet, mkUniqSet, setIdExported, eltsUFM, getUniqSet, emptyInScopeSet, tryEtaReduce, isRuntimeVar, liftIO, manyDataConTy, multiplicityTy, splitForAllTyCoVar, showSDoc, floatBindings, FloatOutSwitches (..), uniqAway, MonadUnique (getUniqueSupplyM, getUniqueM), DynFlags (DynFlags), OccInfo (OneOcc), setNameLoc, extendInScopeList, runCoreM, freeVars, CoreExprWithFVs, DIdSet, FloatBind, wrapFloats, freeVarsOfAnn, freeVarsOf, localiseId, isConLikeId, isDataConId_maybe, UniqSupply, uniqFromSupply)
import GHC.Data.Maybe (fromJust, liftMaybeT)
import GHC.Utils.Outputable (Outputable(ppr))
import GHC.Iface.Ext.Types (pprIdentifier)
import GHC.Tc.Utils.TcType (isTyConableTyVar)
import GHC.Core.Utils (exprType, exprIsExpandable, isExpandableApp)

import qualified Data.Text as T
import Control.Monad.Trans.State
import Data.Data (Data)
import Control.Monad (when)
import Data.Maybe (isNothing, isJust)
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.Map (Map(..), insert, lookup)
import Data.List (intersect, delete, (\\))

import Data.Generics.Uniplate.Data
    ( rewriteBi, transformBi, transformBiM, universeBi, rewriteBiM, Biplate, universe, childrenBi, children )
import GHC.Types.Literal (Literal)
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Tc.Types.Evidence (HoleExprRef(..))
import GHC.Core.Lint (interactiveInScope)
import GHC.Runtime.Context (extendInteractiveContextWithIds)


import Utils 
import Similar ( Similar((~=)), isCommutative )
import Data.Type.Equality (apply)
import qualified Data.Map as M
import GHC.Driver.Monad (modifySession)
import GHC.Core.Opt.Arity (etaExpandAT, exprArity, etaExpand, exprEtaExpandArity, arityTypeArity, findRhsArity)
import GHC.Core.Predicate (isEvVar)
import GHC.Core.Opt.FloatOut ( floatOutwards )
import GHC.Utils.Logger (initLogger)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Core.Type ( Type, Var(..), isLinearType, isTyVar, dropForAlls )
import GHC.Types.Id (setIdArity, setIdInfo, idDataCon, isDataConId_maybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import GHC.Builtin.Uniques (mkBuiltinUnique)
import Analyse (hasCase)
import GHC.Types.Tickish (GenTickish(..))

preProcess :: CoreProgram -> IO CoreProgram 
-- | Preprocessing transformations
preProcess p = removeModInfo p >>= replaceHoles >>= replacePatErrors  

normalise :: String -> CoreProgram -> IO (CoreProgram, Map Var Var) 
-- | Normalising transformations
normalise name p = inlineBinds p >>=
                   recToLetRec >>= 
                   removeRedEqCheck >>= 
                   replaceCaseBinds >>=
                   etaReduce >>= 
                   alphaWCtxt name 

removeModInfo :: CoreProgram -> IO CoreProgram
-- | Remove module information
removeModInfo p = return $ concatMap removeModInf p 
    where removeModInf :: CoreBind -> [CoreBind]
          removeModInf (NonRec v e) | isVarMod v = []
          removeModInf b                         = [b]


replaceHoles :: CoreProgram -> IO CoreProgram 
-- | Replace holes (case typerror) with variables
replaceHoles = return . repHoles
    where repHoles :: CoreProgram -> CoreProgram  
          repHoles = transformBi $ \case
            c@(Case e v t _)
                | isHoleExpr c -> let id = fromJust (getTypErr e)
                                      ty = exprType c
                                      name = makeName "hole" (getUnique id) (getSrcSpan (varName v))
                                      id' = setIdInfo (setVarType (setVarName v name) ty) idinf
                                      idinf = setArityInfo vanillaIdInfo (exprArity e)
                                   in Var (globaliseId id') -- make global Id since hole could be something from any scope
                | otherwise -> c
            e -> e

replacePatErrors :: CoreProgram -> IO CoreProgram 
-- | Replace pattern errors (case patError) with variables
replacePatErrors = return . repPatErr
    where repPatErr :: CoreProgram -> CoreProgram  
          repPatErr = transformBi $ \case
            c@(Case e v t []) -- we only replace paterrors with empty alternatives 
                | isPatError c -> let id = fromJust (getPatErr e)
                                      ty = exprType c
                                      name = makeName "patError" (getUnique id) (getSrcSpan (varName v))
                                      id' = setIdInfo (setVarType (setVarName v name) ty) idinf
                                      idinf = setArityInfo vanillaIdInfo (exprArity e)
                                   in Var (globaliseId id') -- make global Id since hole could be something from any scope
                | otherwise -> c
            e -> e

etaReduce :: CoreProgram -> IO CoreProgram
-- | eta reduction, e.g., \x -> f x => f
etaReduce = return . rewriteBi etaRed
    where etaRed :: CoreExpr ->  Maybe CoreExpr
          {- etaRed (Lam v e) = reduce v e 
          etaRed _         = Nothing  -}
          etaRed (Lam v (Tick t (App f args)))         | isEtaReducable f args v = return (Tick t f)  
          etaRed (Lam v (App f args))                  | isEtaReducable f args v = return f
          etaRed (Lam v (Let b (Tick t (App f args)))) | isEtaReducable f args v = return (Let b (Tick t f))
          etaRed (Lam v (Let b (App f args)))          | isEtaReducable f args v = return (Let b f)
          etaRed _ = Nothing 

{- reduce :: Var -> CoreExpr -> Maybe CoreExpr 
reduce v (Tick _ e)   = reduce v e 
reduce v (Let b e)    = case reduce v e of 
                            Just ex -> return (Let b ex)
                            Nothing -> Nothing 
reduce v (App f args) | isEtaReducable f args v = return f 
reduce _ _ = Nothing  -}
  

isEtaReducable :: CoreExpr -> CoreExpr -> Var -> Bool
isEtaReducable f arg v = case arg of
            Var v' | v == v'
              ,not (isTyVar v)
              ,not (isLinearType (exprType f)) -- don't eta-reduce eta-expanded data constructors (with linear types)
              ,not (ins v f)     -- don't eta reduce if variable used somewhere else in the expression 
              ,not (isEvVar v)   -- don't remove evidence variables 
              -> True
            Tick _ e -> isEtaReducable f e v 
            _ -> False


data VarType = Binder | CaseBind | GenVar

data St = St {
         env  :: Map.Map Var Var
        ,freshCaseBindVar  :: Int
        ,freshVar          :: Int
        ,freshBindVar      :: Int
        ,exerName  :: String
        }

type Ctx a = State St a


initSt :: St
initSt = St {env = Map.empty, freshCaseBindVar = 0, freshVar = 0, freshBindVar = 0, exerName = ""}

alphaWCtxt :: String -> CoreProgram -> IO (CoreProgram, Map.Map Var Var)
-- | Do renaming and return map of renamed variables        
alphaWCtxt fname cs = return (prog, env state)
    where (prog,state) = runState st (initSt {exerName = fname})
          st = mapM alphaR cs

alpha :: String -> CoreProgram -> IO CoreProgram
-- | Do renaming and return map of renamed variables        
alpha fname cs = return $ evalState st (initSt {exerName = fname})
    where st = mapM alphaR cs


alphaR :: CoreBind -> Ctx CoreBind
alphaR (NonRec v e) = do
        v' <- renameVar Binder v
        NonRec v' <$> aRename e
alphaR (Rec es) = do
        vars <- mapM (renameVar Binder . fst) es
        exps <- mapM (aRename . snd) es
        return $ Rec (zip vars exps)

renameVar :: VarType -> Id -> Ctx Id
renameVar vty v = do
            env <- gets env
            name <- gets exerName
            case Map.lookup v env of
                Just n  -> return n
                Nothing -> checkNew v name
    where checkNew v n | varNameUnique v == n = return v -- dont rename main function
                       | isGlobalId v = return v   -- don't rename global ids 
                       | isTyVar v    = return v   -- don't rename type variables 
                       | isTyCoVar v  = return v
                       | isEvVar v    = return v   -- don't rename evidence variables 
                       | isHoleVar v  = return v   -- don't rename hole variables, already have fresh names 
                       | otherwise    = renameV vty v   -- otherwise, rename (might need additional checks)

renameV :: VarType -> Var -> Ctx Var
renameV vty v = do
    vname <- getVName vty
    let v' = setVarName v (makeName vname (getUnique v) (getSrcSpan (varName v)))
    modify $ \s -> s {env = Map.insert v v' (env s)} -- update map 
    return v'

getVName :: VarType -> Ctx String
getVName CaseBind = do
                i <- gets freshCaseBindVar
                modify $ \ s -> s {freshCaseBindVar = i+1}
                return ("cb"++show i)
getVName Binder = do
                i <- gets freshBindVar
                modify $ \ s -> s {freshBindVar = i+1}
                return ("b" ++ show i)
getVName GenVar = do
                i <- gets freshVar
                modify $ \ s -> s {freshVar = i+1}
                return ("v" ++ show i)


aRename :: CoreExpr -> Ctx CoreExpr
aRename v@(Var id)     = Var <$> renameVar GenVar id
aRename t@(Type _)     = return t
aRename l@(Lit _)      = return l
aRename (App e arg)    = do
        e' <- aRename e
        arg' <- aRename arg
        return $ App e' arg'
aRename l@(Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar GenVar b
        e' <- aRename e
        return $ Lam b' e'
aRename c@(Case e v t a) = do
                            e' <- aRename e
                            v' <- renameVar CaseBind v
                            a' <- renameAlt a
                            return $ Case e' v' t a'
aRename (Cast e co)    =
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
            vs' <- mapM (renameVar GenVar) vs
            e' <- aRename e
            return $ Alt ac vs' e'



inlineBinds :: CoreProgram -> IO CoreProgram
-- | Inline recursive binders as let-recs, and non recursive binders directly
inlineBinds = return . inlineBind
    where inlineBind :: CoreProgram -> CoreProgram
          inlineBind [] = []
          inlineBind (b:bs) = case b of
            nr@(NonRec v e) -> if any (insB v) bs then 
                                    let (newBinds, rest) = inline nr bs
                                     in newBinds ++ inlineBind rest 
                            else nr:inlineBind bs 
            rr@(Rec ((v,e):ls)) -> if any (insB v) bs then 
                                    let (newBinds, rest) = inline rr bs
                                     in newBinds ++ inlineBind rest  
                                else rr:inlineBind bs 

inline :: CoreBind -> [CoreBind] -> ([CoreBind],[CoreBind])
-- | Inline a binder and return remaining binders 
inline b bs = let ls  = getBinds bs (getBindTopVar b) -- get all binders using the binder we want to inline 
                  b'  = updateVar (makeLocal (getBindTopVar b)) b -- change scope to local of binder variable if inlined 
                  bs' = insertBind b ls
                  in (bs', delete b (bs \\ ls))

insertBind :: CoreBind -> [CoreBind] -> [CoreBind]
-- | Inline binder in all binders using it 
--  recursive binders are inlined as a let-rec
insertBind n@(NonRec v e) bs = map insertB bs -- inline another nonrec 
    where insertB bi@(NonRec b e') = (transformBi $ \case
            (Var v') | v == v' -> e
            e -> e) bi
          insertB bi@(Rec es) = (transformBi $ \case
            (Var v') | v == v' -> e
            e -> e) bi
insertBind (Rec ((v,e):es)) bs = map insertR bs
    where insertR bind@(NonRec b (Lam x ex)) = NonRec b $ Lam x $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          insertR bind@(NonRec b ex) = NonRec b $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          uv = setVarType (localiseId v) (varType v)
          e' = subst uv v e

recToLetRec :: CoreProgram -> IO CoreProgram 
-- | Inline recursive binders as let-recs when appropriate
recToLetRec p = mkSplitUniqSupply 'H' >>= \ us -> recToLR us p 
    where recToLR :: UniqSupply -> CoreProgram -> IO CoreProgram
          recToLR _ [] = return []
          recToLR us (b:bs) = case b of
            rr@(Rec ((v,e):ls)) -> do -- RECS WITH MORE ITEMS NOT HANDLED
                            let v' = fresh us v
                                e' = subst v' v e
                                b' = NonRec v (Let (Rec ((v',e'):ls)) (Var v'))
                            recToLR us bs >>= \bs' -> return $ b':bs 
            nr@(NonRec v e) -> recToLR us bs >>= \bs' -> return $ nr:bs 

removeRedEqCheck :: CoreProgram -> IO CoreProgram
-- | Remove redundant equality checks
removeRedEqCheck = return . rewriteBi remEqCheck

    where remEqCheck :: CoreExpr ->  Maybe CoreExpr
          -- | remove redundant boolean checks, e.g. 
          -- if x == y then true else false ==> x == y 
          -- f x y | x == y =    True 
          --       | otherwise = False      ==> x == y 
          remEqCheck (Case e v t alt) | isEqCheck e || isNeqCheck e
                                      , all isBoolToBool alt    = Just e
                                      | isEqCheck e
                                      , all isNegBoolToBool alt = Just (replace "==" "/=" e)
                                      | isNeqCheck e
                                      , all isNegBoolToBool alt = Just (replace "/=" "==" e)
          remEqCheck _ = Nothing


isEqCheck :: Data Var => CoreExpr -> Bool
isEqCheck e = or [getOccString v == "==" | Var v <- universe e]

isNeqCheck  :: Data Var => CoreExpr -> Bool
isNeqCheck e = or [getOccString v == "/=" | Var v <- universe e]

isBoolToBool :: Alt Var -> Bool
-- | Case on a bool that also returns a bool
isBoolToBool (Alt (DataAlt d) [] (Var v)) = dstr == vstr &&
                                            dstr == "False" || dstr == "True"
        where dstr = getOccString d
              vstr = getOccString v
isBoolToBool _                            = False

isNegBoolToBool :: Alt Var -> Bool
-- | Case on a bool that also returns a bool, but with reversed logic
isNegBoolToBool (Alt (DataAlt d) [] (Var v)) = (dstr == "False" &&
                                                vstr == "True") ||
                                               (dstr == "True"  &&
                                                vstr == "False")
         where dstr = getOccString d
               vstr = getOccString v
isNegBoolToBool _                            = False


replace :: String -> String -> CoreExpr -> CoreExpr
-- | Replace the variable name with another name, and 
--   update all occurences of the variable in the given expression
replace old new e = subst vnew vold e
    where vold = fromJust $ getVarFromName old e
          vnew = setVarName vold (makeName new (getUnique vold) (mkGeneralSrcSpan "Dummy loc"))


replaceCaseBinds :: CoreProgram -> IO CoreProgram
-- | Substitute back the scrutinee for case binded name in case expressions
replaceCaseBinds = return . transformBi repBinds
    where repBinds :: CoreExpr -> CoreExpr
          -- | replace case result binder with scrutinee 
          --  e.g. Case xs of ys -> {(n:ns) -> f ys} =>  Case xs of ys -> {(n:ns) -> f xs}
          repBinds (Case e b t as) = Case e b t (map (sub e b) as)
              where sub :: CoreExpr ->  Var -> Alt Var -> Alt Var
                    sub e v (Alt ac vars ex) = Alt ac vars (subE e v ex)
          repBinds e = e


removeTyEvidence :: CoreProgram -> CoreProgram
-- | Remove types and type evidence from a Coreprogram
removeTyEvidence = transformBi removeTy 

    where removeTy = \case
            (Lam v e)        | isEvVar v || isTyVar v -> e
            (App f (Var v))  | isEvVar v || isTyVar v -> f
            (App f (Type t)) -> f
            (Let b e) | isEvBind b -> e
            e -> e
          isEvBind (NonRec bi e) = isEvOrTyVar bi && isEvOrTyExp e
          isEvBind (Rec es) = all (isEvOrTyVar . fst) es && all (isEvOrTyExp . snd) es

--- EXPERIMENTAL STUFF BELOW
----------------------------------------------------
----------------------------------------------------


inlineRedLets :: CoreProgram -> IO CoreProgram
-- | Inline redundant let expressions, e.g. let f = x in g f => g x 
inlineRedLets = return . rewriteBi removeLet

removeLet :: CoreExpr ->  Maybe CoreExpr
removeLet (Let (NonRec b ex) e) | isTyApplication ex && isVar innExp = Just $ subst (getVar innExp) b e
    where innExp = getInnerExp ex
          getVar (Var v) = v
removeLet _ = Nothing

isTyApplication :: CoreExpr -> Bool
isTyApplication (App e v) | isEvOrTyExp v = True
                          | otherwise = isTyApplication e

getInnerExp :: CoreExpr -> CoreExpr
getInnerExp (App e v) | isEvOrTyExp v = getInnerExp e
                      | otherwise     = App e v
getInnerExp e = e


floatOutLets :: CoreProgram -> CoreProgram
-- | Float let-binders to toplevel, e.g. f = let g = x in g => x 
floatOutLets = transformBi $ \bind -> case bind :: CoreBind of
     (NonRec v (Lam a (Let b (App (Var v') (Var a'))))) | getBindTopVar b ~= v'
                                                        , a ~= a' -> transformBi (subst v v') (setBindTopVar v b)
     (NonRec v (Let b (Var v'))) | getBindTopVar b == v' -> transformBi (subst v v') (setBindTopVar v b)

     bi -> bi

setBindTopVar :: Var -> CoreBind -> CoreBind
setBindTopVar new (NonRec v e)     = NonRec new e
setBindTopVar new (Rec ((v,e):es)) = Rec ((new,e):es)

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
                b@(NonRec v e) -> NonRec v (eta e df)
                b@(Rec es) -> Rec (etaExp df es)
--
          eta expr df = let arit = exprEtaExpandArity df expr
                        in etaExpandAT arit expr
          etaExp df [] = []
          etaExp df ((v,e):es) = (v,eta e df):etaExp df es

-- Taken from GHC, not exported in 9.2.5
-- in previous version, exprIsExpandable would be the equivalent, but returns true in more cases. 
-- these cases are removed here since a lot of expansions would immediately get reduced again,
-- if also performing eta-reduction.
wantEtaExpansion :: DynFlags -> CoreExpr -> Bool
-- Mostly True; but False of PAPs which will immediately eta-reduce again
-- See Note [Which RHSs do we eta-expand?]
wantEtaExpansion df (Cast e _)             = wantEtaExpansion df e
wantEtaExpansion df (Tick _ e)             = wantEtaExpansion df e
wantEtaExpansion df (Lam b e)              = wantEtaExpansion df e
wantEtaExpansion df (App e _)              = wantEtaExpansion df e
wantEtaExpansion _ (Var v)                 = False
wantEtaExpansion _ (Lit {})                = False
wantEtaExpansion df ex@(Let b e)           = exprArity e < arityTypeArity id_arity
    where  id_arity = findRhsArity df (getBindTopVar b) ex (exprArity ex)
wantEtaExpansion _ _                        = True

wantEtaExpB :: DynFlags -> Var -> CoreExpr -> Bool
wantEtaExpB df v e = id_arity > 0
    where id_arity = arityTypeArity $ findRhsArity df v e ex_arity
          ex_arity = exprArity e


replacePatErrorLits :: CoreProgram -> CoreProgram
-- | Replace patern error literals 
replacePatErrorLits = transformBi $ \case
    c@(Case e v t alt) | isPatError c -> Case (rlit e) v t alt
                       | otherwise -> c
    e -> e
    where rlit = transformBi $ \ex -> case ex :: CoreExpr of
            (Lit (LitString s)) -> Lit (LitString (stripModInfo s))
            e -> e

stripModInfo :: ByteString -> ByteString
-- | Replace pattern error message with location info to dummy string 
stripModInfo s = pack "pattern error"
    -- if wanting location info later 
    --pack $ dropWhile (/= ':') (utf8DecodeByteString s) -- keeping location info

floatOut :: CoreProgram -> Ghc CoreProgram
-- | Using the float out transformation from GHC
floatOut p = do
    df <- getSessionDynFlags
    logger <- liftIO initLogger
    let floatSw = FloatOutSwitches {
            floatOutLambdas = Just 1,    -- float all lambdas to top level,
            floatOutConstants = False,    -- True => float constants to top level,
            floatOutOverSatApps = False,   -- True => float out over-saturated application
            floatToTopLevelOnly = True    -- Allow floating to the top level only.
            }
    us <- liftIO $ mkSplitUniqSupply 'z'
    liftIO $ floatOutwards logger floatSw df us p



addDefaultCase :: CoreProgram -> Ghc CoreProgram
addDefaultCase p = do
    env <- getSession
    let ic = hsc_IC env
    let inscopeVars = mkInScopeSet $ mkUniqSet $ interactiveInScope ic
    transformBiM (addDefCase inscopeVars) p

newGhcVar :: Type -> InScopeSet -> Ghc Id
newGhcVar t is = do
    let uq = unsafeGetFreshLocalUnique is
        name = makeName "fresh" uq (mkGeneralSrcSpan (mkFastString "Dummy location"))
        id'  = mkLocalId name t t  -- reuse id information from top-level binder'
    return id'

addDefCase :: InScopeSet -> CoreBind -> Ghc CoreBind
addDefCase is (NonRec b e)     | not (hasCase e) && not (isEvOrTyVar b) = (addCase is e) >>= \ex -> return $ NonRec b ex
addDefCase is (Rec ((b,e):es)) | not (hasCase e) && not (isEvOrTyVar b) = (addCase is e) >>= \ex -> return $ Rec ((b,ex):es)
addDefCase is b = return b
                        -- | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                       -- needs to use a case rather than let bind

addCase :: InScopeSet -> Expr Var -> Ghc (Expr Var)
addCase is e = do
    let v = getFirstNonTypeLam e
    let t = ft_res $ ft_res (dropForAlls $ exprType e)
    wild <- newGhcVar t is  -- should have same type as the case 
    return $ liftLambdas e (Case (Var v) wild t [Alt DEFAULT [] (innerExp e)])
    where innerExp (Lam v e) = innerExp e
          innerExp e         = e
          liftLambdas (Lam v e) ex = Lam v (liftLambdas e ex)
          liftLambdas _ ex         = ex
          getFirstNonTypeLam (Lam v e) | isEvOrTyVar v = getFirstNonTypeLam e
                                       | otherwise = v
          getFirstNonTypeLam ex = error (show ex)


{- addDefaultCase :: CoreProgram -> CoreProgram
addDefaultCase p = evalState (pm p) initSt
    where pm :: CoreProgram -> Ctx CoreProgram
          pm = transformBiM addDefCase
addDefCase :: Expr Var -> Ctx (Expr Var)
addDefCase ex@(Lam v e) | isTyVar v                     = Lam v <$> addDefCase e
                        | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                       -- needs to use a case rather than let bind
addDefCase e = return e -}
{- 
addCase :: Expr Var -> Var -> Ctx (Expr Var)
addCase e v = do
    let t = varType v
    fresh <- freshVar t
    wild <- freshVar t  -- should have same type as the case 
    e' <- subst_ fresh v e
    return $ Lam fresh $ mkDefaultCase (Var fresh) wild e'
 -}

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
{- caseToGuard :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
caseToGuard = rewriteBi etaRed
ctg :: Expr Var ->  Maybe (Expr Var)
-- | case to guard, e.g. case e of {Just a -> a} => case (e == Just a) of {True -> a}
ctg (Lam v (App f args)) =
   case args of
      Var v' | v == v' -> return f
      _                -> Nothing
ctg _ = Nothing

freshVar :: Type -> Ctx Id
freshVar t = do
    j <- gets freshNum
    let name = makeName ("fresh" ++ show j) (mkUnique 'x' 0) $ mkGeneralSrcSpan (mkFastString "dummy loc")
    let id = mkLocalVar VanillaId name t t vanillaIdInfo
    modify $ \s -> s {env = Map.insert id id (env s), freshNum = j+1} -- update map 
    return id  -}