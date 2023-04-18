
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use lambda-case" #-}

module Transform where

import GHC (HsExpr (..), Name, Ghc, HscEnv, GhcMonad (..), isPrimTyCon, getSessionDynFlags, DynFlags (targetPlatform), isLocalId, SrcSpan)
import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt (..), valArgCount, CoreBind, AltCon (..), isRuntimeArg, AnnExpr' (..))
import GHC.Types.Name ( mkOccName, getOccString, mkInternalName, isDataConName, isSystemName, getSrcSpan)
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
      setVarType, isTyVarBinder, setIdNotExported, mkExportedLocalVar, globaliseId, setVarName )
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Core.Type (eqType, isPiTy, dropForAlls, isForAllTy_co, isForAllTy_ty, splitForAllTyCoVarBinders, splitForAllInvisTVBinders)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.Unique
import GHC.Core.TyCon (TyCon, mkPrelTyConRepName)

import GHC.Types.SrcLoc ( mkGeneralSrcSpan, srcLocSpan, GenLocated )
import GHC.Types.Id.Info (IdDetails(VanillaId), vanillaIdInfo, pprIdDetails, setOccInfo, IdInfo, setArityInfo)
import GHC.Plugins (IdEnv, getInScopeVars, showSDocUnsafe, Literal (LitString), mkDefaultCase, needsCaseBinding, mkLocalId, ModGuts (ModGuts, mg_binds), HscEnv (hsc_IC), InScopeSet, unsafeGetFreshLocalUnique, extendInScopeSet, mkInScopeSet, mkUniqSet, setIdExported, eltsUFM, getUniqSet, emptyInScopeSet, tryEtaReduce, isRuntimeVar, liftIO, manyDataConTy, multiplicityTy, splitForAllTyCoVar, showSDoc, floatBindings, FloatOutSwitches (..), uniqAway, MonadUnique (getUniqueSupplyM, getUniqueM), DynFlags (DynFlags), OccInfo (OneOcc), setNameLoc, extendInScopeList, runCoreM, freeVars, CoreExprWithFVs, DIdSet, FloatBind, wrapFloats, freeVarsOfAnn, freeVarsOf, localiseId)
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
import GHC.Tc.Types.Evidence (HoleExprRef(..))
import GHC.Core.Lint (interactiveInScope)
import GHC.Runtime.Context (extendInteractiveContextWithIds)


import Utils ( isHoleVar, isVarMod, varNameUnique, isHoleExpr, getTypErr, getPatErr, isPatError, getVarFromName )
import Similar ( Similar((~==)) )
import Data.Type.Equality (apply)
import Instance
import qualified Data.Map as M
import Data.Generics.Uniplate (transform)
import GHC.Driver.Monad (modifySession)
import GHC.Core.Opt.Arity (etaExpandAT, exprArity, etaExpand, exprEtaExpandArity, arityTypeArity, findRhsArity)
import GHC.Core.Opt.Simplify.Utils (tryEtaExpandRhs)
import GHC.Types.Tickish (tickishCounts)
import GHC.Core.Predicate (isEvVar)
import GHC.Core.Opt.FloatOut
import GHC.Utils.Logger (initLogger)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Core.Type
import GHC.Types.Id (setIdArity, setIdInfo)
import GHC.Tc.Utils.Env (topIdLvl)
import GHC.Core.Opt.FloatIn (floatInwards)
import GHC.Platform
import Control.Lens.Internal.Zoom (Err(getErr))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import GHC.Builtin.PrimOps (allThePrimOps, PrimOp, primOpOcc)
import GHC.Types.Id.Make (mkPrimOpId)
import GHC.Builtin.Uniques (mkBuiltinUnique)


data St = St {
         env  :: Map.Map Var Var
        ,freshNum  :: Int
        ,exerName  :: String
        }


type Ctx a = State St a

-- \eta -> let f = ... in f eta 
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
      Var v' | not (isTyVar v)
              ,not (isLinearType (exprType f)) -- we cannot eta-reduce applications with linear type constraints
              ,not (ins v f)   -- don't eta reduce if variable used somewhere else in the expression 
              ,not (isEvVar v) -- don't remove evidence variables 
              ,v == v' -> return f 
      _      -> Nothing
etaRed _ = Nothing

ins :: Data (Bind Var) => Var -> Expr Var -> Bool
-- | Find if a variable is used somewhere in an expression
ins v e = or [v==v' | v' <- universeBi e :: [Var]]

makeName :: String -> Unique -> SrcSpan -> Name
-- | Create a name from a string and a variable
--   used for renaming variables
makeName n uq loc = mkInternalName uq (mkOccName Occ.varName n) loc


makeGlobVar :: Unique -> Type -> String -> Id
makeGlobVar uq t n = mkGlobalVar id_det name t id_inf
        where id_det = VanillaId
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              id_inf = vanillaIdInfo




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
                               | otherwise          -> --trace ("arity of " ++ show v `sp` show (arityTypeArity $ findRhsArity df v e (exprArity e)))
                                                       b
                b -> b
          --goE :: DynFlags -> CoreProgram -> CoreProgram
          --goE df = transformBi $ \ex -> case ex :: CoreExpr of
          --  e        | wantEtaExpansion df e  -> eta e df
          --           | otherwise -> e
--
          eta expr df = let arit = exprEtaExpandArity df expr
                        in etaExpandAT arit expr



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
            --arityTypeArity $ exprEtaExpandArity df e


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
  --modifySession $ const env'
  return prog'
        where replaceHoles :: InScopeSet -> CoreProgram -> Ghc (CoreProgram,InScopeSet)
              -- | Replace expressions representing holes with hole variables of the same type 
              replaceHoles is cs = getSessionDynFlags >>= \df -> runStateT (tr cs) is
                where tr :: CoreProgram -> StateT InScopeSet Ghc CoreProgram
                      tr = transformBiM $ \case
                        c@(Case e v t _) | isHoleExpr c -> do
                                               let id = fromJust (getTypErr e)
                                                   ty = exprType c
                                                   name = makeName "hole" (getUnique id) (getSrcSpan (varName v))
                                                   id' = setIdInfo (setVarType (setVarName v name) ty) idinf
                                                   idinf = setArityInfo vanillaIdInfo (exprArity e)
                                               return $ Var (globaliseId id') -- make global Id since hole could be something from any scope

                                         | otherwise -> return c
                        e -> return e

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
    --pack $ dropWhile (/= ':') (utf8DecodeByteString s) -- keeping location info for now, maybe remove later



initSt :: St
initSt = St {env = Map.empty, freshNum = 0, exerName = " "}

alphaWCtxt :: String -> CoreProgram -> (CoreProgram, Map.Map Var Var)
-- | Do renaming and return map of renamed variables        
alphaWCtxt fname cs = (evalState st (initSt {exerName = fname}), map)
    where st@(StateT f) = mapM alphaR cs
          map = env $ execState st (initSt {exerName = fname})

alpha :: String -> CoreProgram -> CoreProgram
-- | Do renaming and return map of renamed variables        
alpha fname cs = evalState st (initSt {exerName = fname})
    where st = mapM alphaR cs

    -- tried uniplate but missed a lot of instances 

alphaR :: Bind Var -> Ctx (Bind Var)
alphaR (NonRec v e) = do
        v' <- renameVar v
        NonRec v' <$> aRename e
alphaR (Rec es) = do
        vars <- mapM (renameVar . fst) es
        exps <- mapM (aRename . snd) es
        return $ Rec (zip vars exps)

renameVar :: Id -> Ctx Id
renameVar v = do
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
                       | otherwise    = renameV v   -- otherwise, rename (might need additional checks)

renameV :: Var -> Ctx Var
renameV v = do
    j <- gets freshNum
    let namestr = "n_"++show j
        name = makeName namestr (getUnique v) (getSrcSpan (varName v))
    let v' = setVarName v name
    modify $ \s -> s {env = Map.insert v v' (env s), freshNum = j+1} -- update map 
    return v'


aRename :: Expr Var -> Ctx (Expr Var)
aRename v@(Var id)     = Var <$> renameVar id
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
            vs' <- mapM renameVar vs
            e' <- aRename e
            return $ Alt ac vs' e'

getTypErrB :: CoreBind -> Maybe Id
getTypErrB (NonRec _ e) = getTypErr e
getTypErrB (Rec es) = case filter isNothing k of
                    [] -> Nothing
                    l  -> Just (fromJust $ head l)
        where k = map (getTypErr . snd) es



rewriteBinds :: String -> CoreProgram -> Ghc CoreProgram
-- | Inline recursive binders as let-recs when appropriate
rewriteBinds fn prog = do
  env <- getSession
  let ic = hsc_IC env
  let inscopeVars = interactiveInScope ic
      is = mkInScopeSet $ mkUniqSet inscopeVars
  (prog',is') <- runStateT (inlineRec is prog) is
  (prog'',is'') <- runStateT (recToNonRec is' prog') is'
  let ic' = extendInteractiveContextWithIds ic (eltsUFM $ getUniqSet $ getInScopeVars is'')
  let env' = env {hsc_IC = ic'}
  setSession env'
  return prog''


recToNonRec :: InScopeSet -> CoreProgram -> StateT InScopeSet Ghc CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder
recToNonRec _ [] = return []
recToNonRec is (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do -- RECS WITH MORE ITEMS NOT HANDLED
                    fresh <- freshGhcVar v
                    let e' = subst fresh v e
                        is' = extendInScopeSet is fresh
                        b' = NonRec v $ (Let (Rec ((fresh,e'):ls)) (Var fresh))
                    put is'
                    recToNonRec is bs >>= \bs' -> return $ b':bs'
    nr@(NonRec v e) -> recToNonRec is bs >>= \bs' -> return $ nr : bs'

inlineRec :: InScopeSet -> CoreProgram -> StateT InScopeSet Ghc CoreProgram
inlineRec is [] = return []
inlineRec is (b:bs) = case b of
    nr@(NonRec v e) -> do
         if any (insB v) bs then do
                                    let (newBinds, rest) = inline nr bs
                                     in inlineRec is rest >>= \bs' -> return $ newBinds ++ bs'
                            else inlineRec is bs >>= \bs' -> return $ nr:bs'
    rr@(Rec ((v,e):ls)) -> if any (insB v) bs then do
                                    let (newBinds, rest) = inline rr bs
                                     in inlineRec is rest >>= \bs' -> return $ newBinds ++ bs'
                            else inlineRec is bs >>= \bs' -> return $ rr:bs'

liftTypConstraints :: Expr Var -> Expr Var
liftTypConstraints (Let (Rec ((b,Lam v e):ls)) ine) | isTyVar v || isEvVar v =
                             Lam v (liftTypConstraints ((Let (Rec ((b,e):ls)) ine)))
liftTypConstraints e = e


freshGhcVar :: Var -> StateT InScopeSet Ghc Id
freshGhcVar id = do
    is <- get
    let uq = unsafeGetFreshLocalUnique is
        name = makeName "fresh" uq (mkGeneralSrcSpan (mkFastString "Dummy location"))
        --ft   = dropForAlls $ snd $ splitForAllInvisTVBinders (varType id)
        id'  = setIdNotExported $ makeLocal $ setVarName id name -- reuse id information from top-level binder
        id'' = topIdLvl
        is'  = extendInScopeSet is id'
    --put is'
    return id'

subst :: Var -> Var -> CoreExpr -> CoreExpr
subst v v' = --trace ("found subst" ++ show "["++ show v' ++ "->" ++ show v ++"]" ) $
             transformBi (sub v v')

sub :: Var -> Var -> CoreExpr -> CoreExpr
-- | Replace the second variable with the first one given
sub v v' = \case
    (Var id) | id == v' -> (Var v)
    e -> e

insB :: Data (Bind Var) => Var -> Bind Var -> Bool
-- | Find if a variable is used somewhere in a binder
insB n b = or [v==n | v <- universeBi b :: [Var]]


inline :: Bind Var -> [Bind Var] -> ([Bind Var],[Bind Var])
-- | Inline a binder and return remaining binders 
inline b bs = let ls  = getBinds bs (getBindTopVar b) -- get all binders using the binder we want to inline 
                  b'  = setBindTopVar (makeLocal (getBindTopVar b)) b -- change scope to local of binder variable if inlined 
                  bs' = insertBind b ls
                  in (bs', delete b (bs \\ ls))

makeLocal :: Var -> Var
makeLocal v | isId v = mkLocalId (varName v) (varMult v) (varType v)
            -- | isId v  && isGlobalId v = mkLocalId (varName v) (varMult v) (varType v)
            -- | isId v                  = v
            -- | otherwise = v


setBindTopVar :: Var -> Bind Var -> Bind Var
setBindTopVar v = transformBi $ \e -> case e :: CoreExpr of
        (Var v') | v == v' -> Var v
        e       -> e

getBindTopVar :: CoreBind -> Var
-- | Get variable of a binder 
getBindTopVar (NonRec v _) = v
getBindTopVar (Rec ((v,e):_)) = v


getBinds :: [CoreBind] -> Var -> [CoreBind]
-- | Get all binders containing a certain variable
getBinds binds v = [r | r <- binds, v `insB` r]


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
insertBind r bs = insertRec r bs 



insertRec :: Bind Var -> [Bind Var] -> [Bind Var]
insertRec (Rec ((v,e):es)) bs = map insertR bs
    where insertR bind@(NonRec b (Lam x ex)) = NonRec b $ Lam x $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          insertR bind@(NonRec b ex) = NonRec b $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          uv = setVarType (localiseId v) (varType v)
          e' = subst uv v e



floatOut :: CoreProgram -> Ghc CoreProgram
floatOut p = do
    df <- getSessionDynFlags
    logger <- liftIO initLogger
    let floatSw = FloatOutSwitches {
            floatOutLambdas = Just 1,    -- float all lambdas to top level,
            floatOutConstants = False,    -- True => float constants to top level,
            floatOutOverSatApps = False,   -- True => float out over-saturated application
            floatToTopLevelOnly = False    -- Allow floating to the top level only.
            }
    us <- liftIO $ mkSplitUniqSupply 'z'
    liftIO $ floatOutwards logger floatSw df us p



removeRedEqCheck :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
-- | Remove redundant equality checks
removeRedEqCheck = rewriteBi remEqCheck

remEqCheck :: Expr Var ->  Maybe (Expr Var)
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


isEqCheck :: Data Var => Expr Var -> Bool
isEqCheck e = or [getOccString v == "==" | Var v <- universe e]

isNeqCheck  :: Data Var => Expr Var -> Bool
isNeqCheck e = or [getOccString v == "/=" | Var v <- universe e]

isBoolToBool :: Alt Var -> Bool 
isBoolToBool (Alt (DataAlt d) [] (Var v)) = dstr == vstr && 
                                            dstr == "False" || dstr == "True"
        where dstr = getOccString d 
              vstr = getOccString v  
isBoolToBool _                            = False 

isNegBoolToBool :: Alt Var -> Bool 
isNegBoolToBool (Alt (DataAlt d) [] (Var v)) = (dstr == "False" && 
                                                vstr == "True") ||
                                               (dstr == "True"  && 
                                                vstr == "False")
         where dstr = getOccString d 
               vstr = getOccString v
isNegBoolToBool _                            = False 


getPrimOp :: String -> Maybe PrimOp   
getPrimOp s = if null ops then Nothing else Just $ head ops 
    where ops = [ op | op <- allThePrimOps, Occ.occNameString (primOpOcc op) == s]


replaceOp :: String -> PrimOp -> CoreExpr -> CoreExpr 
replaceOp s p = transformBi $ \ex -> case ex :: CoreExpr of 
    (Var v) | getOccString v == s -> Var (mkPrimOpId p)
    e -> e 

replace :: String -> String -> CoreExpr -> CoreExpr
replace old new e = subst vnew vold e 
    where vold = fromJust $ getVarFromName old e 
          vnew = setVarName vold (makeName new (getUnique vold) (mkGeneralSrcSpan "Dummy loc")) 


-- Just for testing 

removeEvidence :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
removeEvidence = rewriteBi removeEv 


removeEv :: Expr Var ->  Maybe (Expr Var)
removeEv (Lam v (App f args)) =
   case args of
        Var v' | isEvVar v || isTyVar v -> Just f 
        _                  -> Nothing 
removeEv _ = Nothing


----------------------------------
        -- In reverse dependency order (innermost binder first)


--- EXPERIMENTAL STUFF BELOW
----------------------------------------------------
----------------------------------------------------
{- 
addDefaultCase :: CoreProgram -> CoreProgram
addDefaultCase p = evalState (pm p) initSt
    where pm :: CoreProgram -> Ctx CoreProgram
          pm = transformBiM addDefCase

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

freshVar :: Type -> Ctx Id
freshVar t = do
    (c,i) <- gets freshUq
    j <- gets freshNum
    let name = makeName ("fresh" ++ show j) (mkUnique c i) $ mkGeneralSrcSpan (mkFastString "dummy loc")
    let id = mkLocalVar VanillaId name t t vanillaIdInfo
    modify $ \s -> s {env = Map.insert id id (env s), freshNum = j+1, freshUq=(c,i+1)} -- update map 
    return id  -}