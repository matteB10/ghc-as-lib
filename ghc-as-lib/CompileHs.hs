{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}


module CompileHs where

-- GHC imports 
import GHC
    ( desugarModule,
      getModSummary,
      guessTarget,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      load,
      getSessionDynFlags,
      parenthesizeHsExpr,
      simpleImportDecl,
      parseImportDecl,
      unLoc,
      mkModuleName,
      coreModule,
      ParsedModule(pm_parsed_source),
      TypecheckedMod(typecheckedSource),
      TypecheckedModule(tm_typechecked_source),
      TypecheckedSource,
      Type,
      Name(..),
      GeneralFlag(Opt_KeepHiFiles, Opt_Hpc, Opt_DeferTypedHoles,
                  Opt_DoEtaReduction, Opt_DoLambdaEtaExpansion,
                  Opt_ShowHoleConstraints, Opt_ShowValidHoleFits,
                  Opt_SortValidHoleFits, Opt_SortBySizeHoleFits,
                  Opt_ShowTypeAppOfHoleFits, Opt_ShowTypeOfHoleFits,
                  Opt_ShowProvOfHoleFits, Opt_ShowMatchesOfHoleFits, Opt_OmitYields,
                  Opt_KeepOFiles),
      LoadHowMuch(LoadAllTargets),
      Ghc,
      GhcMonad(getSession),
      DynFlags(generalFlags, refLevelHoleFits, maxValidHoleFits,
               maxRefHoleFits),
      HsModule(hsmodDecls, hsmodImports),
      GhcPs,
      GhcTc,
      ImportDecl(ImportDecl, ideclExt, ideclHiding, ideclAs,
                 ideclImplicit, ideclQualified, ideclSafe, ideclSource,
                 ideclPkgQual, ideclName, ideclSourceSrc),
      EpAnn(EpAnnNotUsed),
      NameAnn,
      SrcSpanAnn'(SrcSpanAnn, locA, ann),
      SrcSpanAnnA,
      InteractiveImport(..),
      RdrName,
      GenLocated(..),
      Id,
      HsBindLR(FunBind, fun_matches),
      LHsDecl,
      GRHSs(GRHSs, grhssGRHSs),
      HsExpr(HsLit, HsUnboundVar, HsPar, HsVar, HsApp),
      HsMatchContext(CaseAlt, FunRhs),
      LGRHS,
      LHsExpr,
      LMatch,
      Match(Match),
      MatchGroup(MG, mg_alts),
      LIdP,
      NoExtField(NoExtField),
      Pat(VarPat), getName, HsParsedModule, ModSummary (..), ModIface )
import GHC.Paths (libdir)
import GHC.Driver.Main 
import GHC.Driver.Session
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.Ppr (showPpr, showSDoc)
import GHC.LanguageExtensions (Extension(..))
import GHC.IORef (newIORef, IORef (..), readIORef)
import GHC.Builtin.Names ( pRELUDE_NAME )
import GHC.Tc.Types.Evidence (HoleExprRef(..), EvTerm (..), EvExpr)
import GHC.Core (CoreExpr)
import qualified GHC.Core.Utils as CoreUtils
import GHC.STRef (STRef(..))
import GHC.Plugins (mkOccName, getOccString, occNameString, VarSet, ModGuts(..), nubSort, HoleFit (RawHoleFit), vanillaIdInfo, mkInternalName, mkGeneralSrcSpan, Var (varType), IdDetails (VanillaId), nameRdrName, Annotation (Annotation), ModGuts (mg_anns), deserializeWithData, fromSerialized, CoreM, flattenBinds, errorMsg, thNameToGhcName, mkRealSrcLoc, HscEnv (..), runHsc, ModSummary, Hsc (Hsc), hsc_home_unit, GenModule (..), realSrcLocSpan, isHoleModule, HasCallStack)
import GHC.Data.Bag (Bag, bagToList, listToBag, emptyBag)
import GHC.Utils.Outputable (Outputable(..), SDoc, showSDocUnsafe, (<+>), ppr, text)

-- General imports 
import System.FilePath (takeBaseName)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Generics.Uniplate.Data ( universeBi, transformBi )
import Data.Data (Data)
import Debug.Trace (trace)
import Data.List (nub)
import Data.List.HT (partitionMaybe)

-- local imports 
import Instance


-- propr imports
import qualified Data.Data.Lens as Lens
import Data.Data.Lens (template, tinplate, uniplate)
import GHC.Tc.Types.Constraint (Cts, Ct (..), ctPred, emptyWC, WantedConstraints (wc_simple), CtEvidence (CtWanted), TcEvDest (HoleDest))
import GHC.Utils.FV
import Control.Lens (Getting, to, universeOf, universeOn, universeOnOf, transformOf, transformOn)
import Control.Lens.Combinators (Fold, contextsOf)
import GHC.Types.Var.Set (isEmptyVarSet, intersectVarSet)
import GHC.Core.TyCo.FVs (tyCoFVsOfType)
import GHC.Core.TyCo.Ppr (appPrec)
import GHC.HsToCore.Monad (initDsWithModGuts)
import GHC.HsToCore.Expr (dsLExpr)
import System.Directory
import System.Environment
import System.IO
import System.Random

import qualified Data.Map as Map
import Numeric (showHex)
import GHC.Tc.Errors.Hole.FitTypes (TypedHole, HoleFit (HoleFit))
import Control.Monad
import Data.Maybe (fromJust, listToMaybe)
import Control.Arrow (first, second, (***))
import Control.Comonad.Store.Class (ComonadStore (peek, pos))
import GHC.Core.TyCo.Rep (Type(..), TyLit (StrTyLit))
import GHC.Data.FastString (fsLit, mkFastString)
import Data.Generics.Biplate (uniplateOnList, universeOn)
import GHC.Tc.Utils.TcType (TcType)
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Var (mkLocalVar)
import GHC.Types.Unique (mkUnique)
import GHC.Tc.Deriv.Generate (mkRdrFunBind)

import Language.Haskell.TH (Q, Exp, runQ, pprint)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Syntax (liftData, Dec (..), AnnTarget (..), Pragma (AnnP), Q (unQ))


import Parsers 
import qualified Data.Text as T
import Utils (showGhcUnsafe, printGhc)



showGhc :: (Outputable a) => DynFlags -> a -> String
showGhc = showPpr


setFlags :: [GeneralFlag]
setFlags = [Opt_Hpc]

exts :: [Extension]
exts = [PartialTypeSignatures, ExtendedDefaultRules]

wFlags :: [WarningFlag]
wFlags = [Opt_WarnOverlappingPatterns]


gen_flags :: [GeneralFlag]
gen_flags = [Opt_DeferTypedHoles,
             Opt_DoEtaReduction,
             Opt_DoLambdaEtaExpansion,
             Opt_ShowHoleConstraints,
             Opt_ShowValidHoleFits,
             Opt_SortValidHoleFits,
             Opt_SortBySizeHoleFits,
             Opt_ShowTypeAppOfHoleFits,
             Opt_ShowTypeOfHoleFits,
             Opt_ShowProvOfHoleFits,
             Opt_ShowMatchesOfHoleFits]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
  -- turn-off all warnings
  flip (foldl wopt_unset) [toEnum 0 ..] $
    flip (foldl xopt_set) exts $
      flip (foldl gopt_set) setFlags $
        (foldl gopt_unset sflags (Opt_OmitYields : gen_flags))
          { maxValidHoleFits = Nothing,
            maxRefHoleFits = Nothing,
            refLevelHoleFits = Just lvl
          }

type HoleExprs = [LHsExpr GhcTc]


compile :: FilePath -> IO ()
compile fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gflags = EnumSet.toList flags ++ gen_flags
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = EnumSet.fromList gflags}
  setSessionDynFlags dflags'
  --imports <- addPreludeIfNotPresent <$> mapM (fmap IIDecl . parseImportDecl) []
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  let parsdecl = Parsers.parseDecl dflags' fp "factorial :: (Eq t, Num t) => t -> t\nfactorial 0 = 1 \nfactorial m = m * factorial (m - 1)" 
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum 
  let (L l hsMod) = pm_parsed_source pmod -- ParsedSource = Located HsModule 
  liftIO $ print hsMod    


compToPars :: FilePath -> IO HsModule
-- | Parse a module
compToPars fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gflags = EnumSet.toList flags ++ gen_flags
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = EnumSet.fromList gflags}
  setSessionDynFlags dflags'
  --imports <- addPreludeIfNotPresent <$> mapM (fmap IIDecl . parseImportDecl) []
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum :: Ghc ParsedModule
  let (L l psrc) = pm_parsed_source pmod -- ParsedSource = Located HsModule 
  let imports = hsmodImports psrc
  let topdecls = hsmodDecls psrc
  return psrc

compToTc :: FilePath -> IO (TypecheckedSource, HoleExprs)
-- | Compile and stop after typechecker 
compToTc fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gflags = EnumSet.toList flags ++ gen_flags 
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        warningFlags = EnumSet.fromList wFlags,
                        generalFlags = EnumSet.fromList gflags}
  setSessionDynFlags dflags'
  imports <- addPreludeIfNotPresent <$> mapM (fmap IIDecl . parseImportDecl) []
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum :: Ghc ParsedModule
  let (L l psrc) = pm_parsed_source pmod -- ParsedSource = Located HsModule 
  let imports = hsmodImports psrc
  let topdecls = hsmodDecls psrc
  liftIO $ banner "topDecls"
  liftIO $ print topdecls
  tmod <- typecheckModule pmod
  env' <- getSession 
  let tenv = hsc_type_env_var env'
  case tenv of 
    Just (m,te) -> do
      r <- liftIO $ readIORef te 
      liftIO $ printGhc r
    Nothing -> liftIO $ putStrLn "No TypeEnv"
  let tprogram = tm_typechecked_source tmod
  liftIO $ putStrLn $ showSDocUnsafe $ ppr tprogram
  liftIO $ banner "typecheckde"
  liftIO $ print tprogram
  let holes = extractHoles tprogram
  let lits = extractLits tprogram
  let ref = exHoleLits holes
  --r <- liftIO $ readIORef $ head ref 
  --liftIO $ putStrLn $ "refterm:" ++ showSDocUnsafe (ppr r)
  liftIO $ banner "Literals"
  liftIO $ print lits
  liftIO $ banner "Holes"
  liftIO $ putStrLn $ "nb of holes: " ++ show (length holes) ++ " holes:" ++ show holes
  liftIO $ banner "Holes"
  fits <- getExprFitCands tmod
  let holetypes = concatMap extractHoleTypes holes
  liftIO $ banner "holetypes"
  liftIO $ putStrLn $ showSDocUnsafe $ ppr holetypes
  hscenv <- getSession 
  --liftIO $ banner "getinstances for type"
  --cand <- getInstancesForType (head holetypes) 
  --liftIO $ putStrLn $ showSDocUnsafe $ ppr cand 
  --liftIO $ putStrLn $ showSDocUnsafe $ ppr (map efc_cand fits) 
  return (tprogram, holes)
 

addPreludeIfNotPresent :: [InteractiveImport] -> [InteractiveImport]
addPreludeIfNotPresent decls =
  if any isPrelude decls
    then decls
    else prelImport : decls
  where
    isPrelude (IIModule mname) = mname == pRELUDE_NAME
    isPrelude (IIDecl ImportDecl {..}) = unLoc ideclName == pRELUDE_NAME
    prelImport = IIDecl $ simpleImportDecl pRELUDE_NAME

banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"


extractHoleTypes ::  LHsExpr GhcTc -> [TcType]
extractHoleTypes expr = case expr of
    L l (HsUnboundVar hole@(HER ior t u) on) -> [t]
    _                                        -> []


extractHoles :: TypecheckedSource -> [LHsExpr GhcTc]
extractHoles b = holes
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            holes = [L l (HsUnboundVar hole on) | L l (HsUnboundVar hole@(HER _ t u) on) <- exps]

exHoleLits :: [LHsExpr GhcTc] -> [IORef EvTerm]
exHoleLits e = [ior | L l (HsUnboundVar hole@(HER ior t u) on) <- e]

extractLits :: TypecheckedSource -> [LHsExpr GhcTc]
extractLits b = lits
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            lits = [L l (HsLit li a) | L l (HsLit li a) <- exps]

{- 
compToTctest :: FilePath -> IO (CoreProgram, [Warning])
-- | Compile and stop after typechecker 
compToTctest fp = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gflags = EnumSet.toList flags ++ genFlags
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = EnumSet.fromList gflags}
  setSessionDynFlags dflags'
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  target <- guessTarget fp Nothing
  loadWithPlugins dflags' target [StaticPlugin $ PluginWithArgs
            { paArguments = [],
              paPlugin = plugin -- hlint plugin
            }] 
  
  --load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum 
  let pprogram = attachNoteP $ pm_parsed_source pmod
  
  let pticks = extractTicksP pprogram 
  --let holes = extractUn pprogram 
  --liftIO $ print holes 
  --liftIO $ putStrLn $ showGhcUnsafe pprogram
  --liftIO $ print pprogram  
  --liftIO $ print $ map showParsedExpr pticks
  tmod <- typecheckModule pmod 
  let tprogram = attachNote $ tm_typechecked_source tmod
  liftIO $ print $ tm_typechecked_source tmod 
  liftIO $ print $ extractTicksT (tm_typechecked_source tmod )
  liftIO $ print $ map sourceName (extractTicksT tprogram)
  dmod <- desugarModule (tmod {tm_typechecked_source = tprogram})
  let cmod = dm_core_module dmod 
  liftIO $ print (mg_binds cmod)
  warns <- liftIO $ readIORef ref  
  let coreticks = getTicks (mg_binds cmod )
  liftIO $ print coreticks 
  liftIO $ print (map sourceName coreticks)
  let warninglocations = map realSrcSpan (getWarnLoc warns )
  cprog <- liftIO $ preProcess (mg_binds cmod) >>= normalise "dupli"
  return (cprog, nub warns) -}
{- rewriteGuards :: Data (GRHS a b) => TypecheckedSource -> TypecheckedSource
rewriteGuards = transformBi $ \expr -> case expr of 
  L FunBind {fun_matches = MG {mg_alts = L  [L  (Match {m_ctxt = FunRhs {..}, m_pats = [p], m_grhss = GRHSs {grhssGRHSs = [L a GRHS [] b]}})]}} [] -> undefined 
 -}


   {-  transformBi $ \expr -> 
   case expr :: HsExpr GhcTc of
      (HsUnboundVar x@(HER _ t u) n) -> do 
                                    return $ HsUnboundVar x n   -- hole found
      e -> return e -- want to continue to recursively search for holes without pattern match on all expressions, how to do that? -}

-- type family XUnboundVar x
-- type instance XUnboundVar GhcTc = HoleExprRef = HER type name 
-- HsUnboundVar (XUnboundVar x) (OccName) -- the unboundvar can be a hole, but how can we pattern match on it?
-- | Where to store evidence for expression holes
-- See Note [Holes] in GHC.Tc.Types.Constraint
--data HoleExprRef = HER (IORef EvTerm)   -- ^ where to write the erroring expression
--                       TcType           -- ^ expected type of that expression
--                       Unique           -- ^ for debug output only

-- =====================
-- Borrowed and modified from PropR

-- | Takes an expression and generates HoleFitCandidates from every subexpresson.
-- Uses either the expression itself or the desugared module
-- THIS FUNCTION RETURNS REPAIR CANDIDATES, NOT FIT CANDIDATES
getExprFitCands :: TypecheckedModule -> Ghc [ExprFitCand]
getExprFitCands tmod = do
  liftIO $ putStrLn "Getting expression fit cands..."
  -- setSessionDynFlags reads the package database.
  liftIO $ putStrLn "Reading the package database..."
  _ <- setSessionDynFlags =<< getSessionDynFlags
  -- If it type checks, we can use the expression
  liftIO $ putStrLn "Typechecking the expression..."
  let exprs :: [LHsExpr GhcTc]
      exprs = universeOnOf tinplate Lens.uniplate $ typecheckedSource tmod
          -- TODO: is it OK to ignore the wcs here? Should be.
      esAndNames = toEsAnNames emptyWC $ filter nonTriv exprs
  desugared <- desugarModule tmod
  liftIO $ putStrLn "Getting the session..."
  hsc_env <- getSession
      -- After we've found the expressions and any ids contained within them, we
      -- need to find their types
  liftIO $ putStrLn "Getting expression types..."
  mb_tys <- liftIO $ mapM
            ( fmap (fmap CoreUtils.exprType . snd)
                . initDsWithModGuts hsc_env (coreModule desugared)
                . dsLExpr
                . (\(e, _, _) -> e)
            )
            esAndNames
  return $ zipWith finalize esAndNames mb_tys
  where
    toEsAnNames wc = map (\e -> (e, bagToList $ wc_simple wc, concatMap e_ids $ flattenExpr e))
    e_ids (L _ (HsVar _ v)) = [unLoc v]
    --e_ids (L _ (HsUnboundVar hole@(HER ior t u) on)) =  undefined
    e_ids _ = []
    --e_ids (L _ (HsWrap _ v)) = concatMap e_ids $ flattenExpr $ noLoc v -- expression cannot be wraps 
    -- Vars are already in scope
    nonTriv :: LHsExpr GhcTc -> Bool
    nonTriv (L _ HsVar {}) = False
    -- We don't want more holes
    nonTriv (L _ HsUnboundVar {}) = True
    -- We'll get whatever expression is within the parenthesis
    -- or wrap anyway
    nonTriv (L _ HsPar {}) = False
    --nonTriv (L _ HsWrap {}) = False
    nonTriv _ = True
    finalize :: (LHsExpr GhcTc, [Ct], [Id]) -> Maybe Type -> ExprFitCand
    finalize (e, _, rs) ty@Nothing = EFC (parenthesizeHsExpr appPrec e) emptyBag rs ty
    finalize (e, wc, rs) ty@(Just expr_ty) =
      EFC (parenthesizeHsExpr appPrec e) (listToBag (relevantCts expr_ty wc)) rs ty
    -- Taken from TcHoleErrors, which is sadly not exported. Takes a type and
    -- a list of constraints and filters out irrelvant constraints that do not
    -- mention any typve variable in the type.
    relevantCts :: Type -> [Ct] -> [Ct]
    relevantCts expr_ty simples =
      if isEmptyVarSet (fvVarSet expr_fvs')
        then []
        else filter isRelevant simples
      where
        ctFreeVarSet :: Ct -> VarSet
        ctFreeVarSet = fvVarSet . tyCoFVsOfType . ctPred
        expr_fvs' = tyCoFVsOfType expr_ty
        expr_fv_set = fvVarSet expr_fvs'
        anyFVMentioned :: Ct -> Bool
        anyFVMentioned ct =
          not $
            isEmptyVarSet $
              ctFreeVarSet ct `intersectVarSet` expr_fv_set
        -- We filter out those constraints that have no variables (since
        -- they won't be solved by finding a type for the type variable
        -- representing the hole) and also other holes, since we're not
        -- trying to find hole fits for many holes at once.
        isRelevant ct =
          not (isEmptyVarSet (ctFreeVarSet ct))
            && anyFVMentioned ct
            -- && not (isHoleCt ct) 

--flattenExpr :: Data (HsExpr id) => LHsExpr id -> [LHsExpr id]
flattenExpr = universeOf Lens.uniplate

data ExprFitCand = EFC
  { efc_cand :: LHsExpr GhcTc,
    efc_wc :: Cts,
    efc_ids :: [Id],
    efc_ty :: Maybe Type
  }

instance Show ExprFitCand where
  show (EFC c w id t) = show c


{- data MatchGroup p body
  = MG { mg_ext     :: XMG p body -- Post-typechecker, types of args and result
       , mg_alts    :: XRec p [LMatch p body]  -- The alternatives
       , mg_origin  :: Origin } -}


patToGuard :: (Data (HsBindLR GhcPs GhcPs)) => HsModule -> HsModule
patToGuard = transformBi $ \e -> case e :: HsBindLR GhcPs GhcPs of
     f@(FunBind {fun_matches = mg}) -> f  {fun_matches = rewriteFbCase mg}


decls :: HsModule -> [LHsDecl GhcPs]
decls = hsmodDecls


-- | Rewrite function bindings to a pattern binding with a case:
--   f x = 1 ; f (x:xs) = 1 + f xs   =>   f = \x -> case x of x -> 1 ; (x:xs) -> 1 + f xs
rewriteFbCase :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
rewriteFbCase = \case
           m@MG {mg_alts = L s alts} -> m {mg_alts = L s (rewriteAlts alts)}

rewriteAlts :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
rewriteAlts [] = []
rewriteAlts ((L s (Match p f@(FunRhs {}) [L sp b]  r)):ms) = L s (Match p f [L sp (VarPat NoExtField fresh)] (rr r)):rewriteAlts ms
rewriteAlts ((L s (Match p CaseAlt [L sp b] r)):ms) = undefined

rr :: GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))
rr g@GRHSs{grhssGRHSs = l} = g {grhssGRHSs = map match l}
  where
    match :: LGRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> LGRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))
    match = transformBi $ \v -> case v :: (HsExpr GhcPs) of  -- if no guarded statements 
        (HsVar n (L sp name)) -> HsVar n fresh
        (HsApp l (L s (HsVar n f)) (L s' (HsVar n2 f2))) -> HsApp l (L s (HsVar n f)) (L s' (HsVar n2 f2))
        e -> e

fresh2 :: LIdP GhcPs -> LIdP GhcPs
fresh2 (L srcAnn n) = fresh

fresh :: LIdP GhcPs
fresh = L srcAnn (makeId "fresh")
      where srcAnn = SrcSpanAnn { ann = ep, locA = mkGeneralSrcSpan (mkFastString ("Loc "))}
            ep :: EpAnn NameAnn
            ep = EpAnnNotUsed


makeId :: String -> RdrName
makeId n = nameRdrName name
        where uq     = mkUnique 'a' 1
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))


{- rewriteFbsCase :: Q Exp -> Q Exp 
rewriteFbsCase d@(TH.FunD name clauses) = do
      names <- replicateM (length pats) (TH.newName "x")
      return $ TH.FunD name $ TH.Clause [] TH.NormalB $ TH.LamE (map TH.VarP names) 
                                                        (TH.CaseE (TH.TupE $ map (Just . TH.VarE) names) (map match pats))
                                                  
  where
    pats = concatMap getPats clauses  -}
                    {- $ (TH.LamE (map TH.VarP ids) 
                            (TH.CaseE (TH.TupE $ map (Just . TH.VarE) ids) 
                                  (pats))) []-}



---- 

