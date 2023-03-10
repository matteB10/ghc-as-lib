{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module HMain where 

-- GHC imports 
import GHC 
import GHC.Paths (libdir)
import GHC.Driver.Main 
import GHC.Driver.Session
import qualified GHC.Data.EnumSet as EnumSet 
import GHC.Utils.Outputable (Outputable (ppr), showSDocUnsafe)
import GHC.Driver.Ppr (showPpr)
import GHC.LanguageExtensions (Extension(..))
import GHC.IORef (newIORef, IORef (..), readIORef)
import GHC.Builtin.Names
import GHC.Tc.Types.Evidence (HoleExprRef(..), EvTerm (..), EvExpr)
import GHC.Core (CoreExpr)
import qualified GHC.Core.Utils as CoreUtils 
import GHC.STRef (STRef(..))
import GHC.Plugins (mkOccName, getOccString, occNameString, VarSet, nubSort, HoleFit (RawHoleFit))
import GHC.Data.Bag (Bag, bagToList, listToBag, emptyBag)

-- General imports 
import System.FilePath (takeBaseName)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Generics.Uniplate.Data ( universeBi )  
import Data.Data (Data)
import Debug.Trace (trace)
import Data.List (nub)

-- local imports 
import Instance 



-- propr imports
import qualified Data.Data.Lens as Lens 
import Data.Data.Lens (template, tinplate, uniplate)
import GHC.Tc.Types.Constraint (Cts, Ct (..), ctPred, emptyWC, WantedConstraints (wc_simple), CtEvidence (CtWanted), TcEvDest (HoleDest))
import GHC.Utils.FV 
import Control.Lens (Getting, to, universeOf, universeOn, universeOnOf, transformOf)
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
import Data.Maybe (fromJust)
import Control.Arrow (first, second, (***))
import Control.Comonad.Store.Class (ComonadStore (peek, pos))



showGhc :: (Outputable a) => DynFlags -> a -> String
showGhc = showPpr 


setFlags :: [GeneralFlag]
setFlags = [Opt_Hpc]

exts :: [Extension]
exts = [PartialTypeSignatures, ExtendedDefaultRules]


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

type Holes = [LHsExpr GhcTc]

compToTc :: FilePath -> IO (TypecheckedSource, Holes)
compToTc fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gflags = EnumSet.toList flags ++ gen_flags
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
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
  --v <- compileParsedExpr psrc
  tmod <- typecheckModule pmod
  -- tm_renamed_source :: Maybe RenamedSource
  let tprogram = tm_typechecked_source tmod 
  liftIO $ putStrLn $ showSDocUnsafe $ ppr tprogram 
  liftIO $ banner "typecheckde"
  liftIO $ print tprogram
  let holes = extractHoles tprogram 
  let lits = extractLits tprogram 
  let ref = exHoleLits holes 
  r <- liftIO $ readIORef $ head ref 
  liftIO $ putStrLn $ "refterm:" ++ showSDocUnsafe (ppr r)
  liftIO $ banner "Literals"
  liftIO $ print lits 
  liftIO $ banner "Holes"
  liftIO $ putStrLn $ "nb of holes: " ++ show (length holes) ++ " holes:" ++ show holes  
  liftIO $ banner "HoleFits"
  fits <- getExprFitCands tmod 
  liftIO $ putStrLn $ showSDocUnsafe $ ppr (map efc_cand fits) 
  return (tprogram, holes) 

-- type TypecheckedSource = LHsBinds GhcTc 
--                        = LHsBindsLR id id
--                        = Bag (LHsBindLR idL idR)
--                        = Bag (XRec idL (HsBindLR idL idR))
-- HsBindLR idL idR = FunBind ... | PatBind ... 


addPreludeIfNotPresent :: [InteractiveImport] -> [InteractiveImport]
addPreludeIfNotPresent decls =
  if any isPrelude decls
    then decls
    else prelImport : decls
  where
    isPrelude (IIModule mname) = mname == pRELUDE_NAME
    isPrelude (IIDecl ImportDecl {..}) = unLoc ideclName == pRELUDE_NAME
    isPrelude _ = False
    prelImport = IIDecl $ simpleImportDecl pRELUDE_NAME

banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"


extractHoles :: Bag (GenLocated SrcSpanAnnA (HsBindLR GhcTc GhcTc)) -> [LHsExpr GhcTc]
extractHoles b = holes
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            holes = [L l (HsUnboundVar hole on) | L l (HsUnboundVar hole@(HER _ t u) on) <- exps]

exHoleLits :: [LHsExpr GhcTc] -> [IORef EvTerm]
exHoleLits e = [ior | L l (HsUnboundVar hole@(HER ior t u) on) <- e]

extractLits :: Bag (GenLocated SrcSpanAnnA (HsBindLR GhcTc GhcTc)) -> [LHsExpr GhcTc]
extractLits b = lits 
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            lits = [L l (HsLit li a) | L l (HsLit li a) <- exps]
    
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
