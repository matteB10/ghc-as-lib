{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- disable warnings from unrecognised pragma in model solution files 
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-all #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Compile where

-- | Main module for compiling to different compilation passes 


import GHC
    ( defaultErrorHandler,
      desugarModule,
      getBindings,
      getModSummary,
      getModuleGraph,
      guessTarget,
      loadModule,
      modInfoExports,
      modInfoTyThings,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      load,
      getSessionDynFlags,
      getNamesInScope,
      mkModuleName,
      coreModule,
      ParsedMod(parsedSource),
      ParsedModule(..),
      TypecheckedMod(moduleInfo),
      TypecheckedModule(tm_typechecked_source, tm_renamed_source, TypecheckedModule, tm_internals_),
      LoadHowMuch(LoadAllTargets),
      GhcMonad(getSession, setSession),
      DynFlags(..),
      SuccessFlag,
      DesugaredModule (dm_core_module, dm_typechecked_module),
      compileToCoreSimplified,
      parseExpr,
      CoreModule (cm_binds, cm_types), pprVarSig, compileToCoreModule, SrcLoc, getContext, getRealSrcSpan, lookupName, TypecheckedSource, gopt, ModSummary (ModSummary), ParsedModule (ParsedModule), Target, Severity (..), showGhcException, GhcException, defaultWarnErrLogger, pushLogHookM, mkHsDocString, popLogHookM)

--import GHC.Show
import GHC.Paths (libdir)
import GHC.Data.Bag ()

import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut, gopt_set, optimisationFlags, dopt_set, WarningFlag (Opt_WarnUnrecognisedPragmas, Opt_WarnTypedHoles, Opt_WarnInlineRuleShadowing, Opt_WarnOverlappingPatterns, Opt_WarnRedundantConstraints, Opt_WarnIncompletePatterns, Opt_WarnInaccessibleCode), gopt_unset, wopt_unset, initSDocContext, wopt_set)
import GHC.Driver.Flags ( DumpFlag(..), GeneralFlag (..), WarnReason (..))
import GHC.Utils.Outputable ( Outputable(..), showSDocUnsafe, defaultErrStyle, defaultSDocContext)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Unit.Module.ModGuts (mg_binds, ModGuts (mg_module, mg_binds))
import GHC.Core ( Bind(..), Expr(..), CoreProgram, Alt(..), AltCon(..), CoreBind(..), CoreExpr, CoreBndr)
import GHC.Driver.Ppr (showPpr)
import GHC.CoreToStg (coreToStg)
import GHC.Types.SourceError ( handleSourceError, SourceError (SourceError))
import GHC.Types.Error (getErrorMessages, MsgEnvelope, mkLocMessage, WarningMessages)
import GHC.Core.DataCon (DataCon(..), mkDataCon, dataConName)
import GHC.Types.Var (Var(..), TyCoVarBinder(..), VarBndr(..), ArgFlag(..), AnonArgFlag, TyCoVar, Specificity(..), idDetails, setIdExported)
import GHC.Types.Literal (Literal(..), LitNumType, pprLiteral)
import GHC.Types.Basic (FunctionOrData (IsFunction, IsData))
import GHC.Types.Tickish ( CoreTickish )
import GHC.Core.TyCon (TyCon(..))
import GHC.Core.TyCo.Rep (Type(..), CoercionR, TyLit(..), TyCoBinder, mkTyVarTy)
import GHC.Types.Name (Name(..), isHoleName, nameStableString, OccName, getSrcLoc, NamedThing (getName), pprOccName, pprDefinedAt, getOccString, nameUnique, HasOccName (occName), isDataConName, isTyConName, isTyVarName, isSystemName)
import GHC.Tc.Types (TcGblEnv(..))
import GHC.Core.Lint

import GHC.Tc.Errors.Hole ()
import GHC.Tc.Errors.Hole.FitTypes ()
import GHC.Driver.Env (HscEnv(hsc_plugins, hsc_static_plugins, hsc_dflags, hsc_logger, HscEnv, hsc_IC, hsc_NC), runHsc, Hsc(..))
import GHC.Driver.Monad (modifySession, Ghc, putMsgM, liftGhcT, withSession, Session)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Tc.Types.Constraint (Cts(..), Implication(..), Hole(..), HoleSort(..), CtLoc)
import GHC.Types.Var.Set (VarSet, pprVarSet)
import GHC.Types.Unique.Set (UniqSet(..), pprUniqSet)
import GHC.Core.TyCo.Ppr (pprTyVar)
import GHC.Tc.Types.Evidence (HoleExprRef)
import GHC.Tc.Utils.TcType (TcLevel)
import GHC.Tc.Types.Origin (SkolemInfo)
import GHC.Unit.Module.Warnings (Warnings (..), pprWarningTxtForMsg)
import GHC.Types.TyThing (TyThing(..))
import GHC.Core.Coercion.Axiom (CoAxiom)
import GHC.Core.ConLike (ConLike)
import GHC.Data.FastString (fsLit, mkFastString)

import System.FilePath ( replaceDirectory, takeBaseName )
import Debug.Trace (trace)
import System.Posix.Internals (puts)
import Control.Monad (when, unless)

import Similar
import Instance
import Transform (etaReduce, alpha, removeModInfo, repHoles, etaExpP, recToLetRec, inlineBinds, floatOut, replacePatErrorLits, removeRedEqCheck, removeTyEvidence, recToLetRec, floatOutLets, addDefaultCase, replaceCaseBinds, aRename, inlineRedLets)
import Utils
    ( banner, findLiterals, printHoleLoc, showGhc, ExerciseName, getExerciseName )
import GHC.Core.Opt.Monad (CoreToDo (..), getRuleBase, CoreM, SimplMode (sm_pre_inline))
import GHC.Core.Opt.Pipeline (core2core)
import GHC.Iface.Ext.Utils (getNameScope)
import GHC.Core.Opt.Simplify.Env (getSimplRules)
import GHC.Driver.Plugins
import MyPlugin (plugin, install)
import GHC.Plugins (mkModuleNameFS, ModGuts (ModGuts, mg_warns), Unique, mkLocalVar, IdDetails (VanillaId), mkInternalName, mkOccName, mkGeneralSrcSpan, vanillaIdInfo, mkInScopeSet, mkUniqSet, MonadUnique (getUniqueM), setVarType, extendInScopeSet, unsafeGetFreshLocalUnique, moduleEnvElts, InScopeSet, getInScopeVars, getUniqSet, eltsUFM, mkGlobalVar, isFunTy, Uniquable (getUnique), idInfo, exprType, srcErrorMessages, runSDoc, handleGhcException, showException, SrcSpan, SDoc, docToSDoc)
import Control.Monad.Trans.State (StateT (runStateT), get, put, evalStateT)
import qualified GHC.Types.Name.Occurrence as Occ
import Data.Generics.Uniplate.Data
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)
import GHC.Runtime.Context (extendInteractiveContextWithIds, icInScopeTTs)
import Data.Data (Data)
import Data.IORef (readIORef, IORef, modifyIORef', newIORef, modifyIORef)
import GHC.Types.Name.Cache (NameCache(..))
import GHC.Core.Unfold (UnfoldingOpts(..))
import Control.Monad.Catch as MC ( MonadCatch(catch), SomeException (SomeException), handle, finally )
import TypeCheckW (hscTypecheckAndGetWarnings)
import Data.Set
import GHC.IO (catchException, catchAny)
import GHC.Driver.Errors (handleFlagWarnings)
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Utils.Logger (LogAction)
import Data.Function (on)
import Data.List (nubBy, nub)
import qualified GHC.Utils.Ppr as Pretty


  {-
  GeneralFlag's regarding hole fits 
   | Opt_ShowValidHoleFits
   | Opt_SortValidHoleFits
   | Opt_SortBySizeHoleFits
   | Opt_SortBySubsumHoleFits
   | Opt_AbstractRefHoleFits
   | Opt_UnclutterValidHoleFits
   | Opt_ShowTypeAppOfHoleFits
   | Opt_ShowTypeAppVarsOfHoleFits
   | Opt_ShowDocsOfHoleFits
   | Opt_ShowTypeOfHoleFits
   | Opt_ShowProvOfHoleFits
   | Opt_ShowMatchesOfHoleFits
  -}

holeFlags :: [GeneralFlag]
-- | General flags concerning typed holes 
holeFlags =
  [ Opt_ShowHoleConstraints,
    Opt_ShowProvOfHoleFits,
    Opt_ShowTypeAppVarsOfHoleFits,
    Opt_ShowTypeAppOfHoleFits,
    Opt_ShowTypeOfHoleFits,
    Opt_SortBySizeHoleFits,
    Opt_ShowMatchesOfHoleFits,
    Opt_UnclutterValidHoleFits
  ]

simplFlags :: [GeneralFlag]
-- | Set flags for simplification pass  
simplFlags = [
             --Opt_LiberateCase
            --,Opt_CaseFolding
            Opt_DoLambdaEtaExpansion,
            -- ,Opt_Specialise
            --,Opt_CaseMerge
            --,Opt_DoEtaReduction
            Opt_EnableRewriteRules
             ]

genFlags :: [GeneralFlag]
genFlags = [
           -- Opt_WarnIsError  -- turn all warnings into errors 
            Opt_DeferTypedHoles
           ,Opt_DeferTypeErrors
           ,Opt_DoCoreLinting
           ,Opt_DeferDiagnostics
           ]

setWarnFlags :: [WarningFlag]
setWarnFlags = [Opt_WarnOverlappingPatterns
                ,Opt_WarnIncompletePatterns
                ,Opt_WarnInaccessibleCode
                ,Opt_WarnRedundantConstraints]

unSetWarnFlags :: [WarningFlag]
unSetWarnFlags = [Opt_WarnUnrecognisedPragmas
                 ,Opt_WarnTypedHoles
                 ,Opt_WarnInlineRuleShadowing
                 ]

setFlags :: Bool -> [GeneralFlag] -> Ghc DynFlags
setFlags b flags = do
   df <- getSessionDynFlags
   let dflags = Prelude.foldl gopt_unset df [Opt_KeepHiFiles, Opt_KeepOFiles]
       dflags' = Prelude.foldl wopt_unset dflags unSetWarnFlags
       gflags' = if b then EnumSet.toList (generalFlags dflags') ++ flags else flags
       dflags1 = dflags' {refLevelHoleFits = Just 2,
                          maxValidHoleFits = Just 8,
                          maxRefHoleFits   = Just 10,
                          generalFlags = EnumSet.fromList gflags'}
       dflags2 = Prelude.foldl wopt_set dflags1 setWarnFlags
   return dflags2


compToFile :: (FilePath -> IO CoreProgram) -> FilePath -> IO ()
-- | Compile with given compile function and write to file 
compToFile compile file = do
  coreprog <- compile file
  let file_out = replaceDirectory file "./out/"
  liftIO $ writeFile file_out $ show coreprog


typeCheckCore :: CoreProgram -> HscEnv -> IO ()
-- | Use Core Linter to check for problems
typeCheckCore coreprog env = do
   let coretodo = CoreDoPasses [CoreDoNothing, CoreTidy]
       dflags = hsc_dflags env
   unless (gopt Opt_DoCoreLinting dflags) $ error "CoreLinting flag must be set"
   liftIO $ lintPassResult env coretodo (coreprog)




compCoreSt :: Bool -> FilePath -> Ghc (CoreProgram, HscEnv)
-- | Compile to desugaring pass 
compCoreSt b fp = do
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  env <- getSession
  dflags <- setFlags b (holeFlags ++ genFlags)
  setSessionDynFlags (gopt_set dflags Opt_DoCoreLinting)
  coremod <- compileToCoreModule fp
  env' <- getSession
  return (cm_binds coremod, env')


data Warning = GhcWarn {reason :: WarnReason,
                        sev :: Severity,
                        span :: SrcSpan,
                        doc :: SDoc}
  deriving Show 

instance Eq WarnReason where
  (Reason flag) == (Reason flag') = flag == flag'
  (ErrReason f) == (ErrReason f') = f == f'
  NoReason == NoReason = True
  _ == _               = False

instance Eq Warning where 
  GhcWarn r s sp doc == GhcWarn r' s' sp' doc' = r == r' && sp == sp' -- for checking the same program, comparing warnings from model/student requires other handling

instance Ord Severity where 
  SevFatal > _   = True 
  SevError > x   = x /= SevFatal 
  SevWarning > x = x /= SevError 
  


writeWarnings :: IORef [Warning] -> LogAction -> LogAction
writeWarnings ref action dflags reason sev span doc = do
  modifyIORef ref (GhcWarn reason sev span doc:)
  action dflags reason sev span doc --(docToSDoc Pretty.empty) -- dont print msg again 


compSimpl :: ExerciseName -> FilePath -> IO (CoreProgram, HscEnv, [Warning])
-- | Compile coreprogram, after simplifier pass 
--   True to use the default optimisation flags 
compSimpl name fp = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
  env <- getSession
  dflags <- setFlags True (holeFlags ++ genFlags ++ simplFlags) -- set dynflags 
  setSessionDynFlags dflags
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum
  tmod <-  typecheckModule pmod
  dmod <- desugarModule tmod
  let coremod = coreModule dmod
      p = removeModInfo (mg_binds coremod)
      p1 = repHoles p
  env <- getSession
  p2 <- liftIO $ core2core env (coremod {mg_binds = p1})
  let p3 = mg_binds p2 
  p4 <- inlineBinds p3
  p5 <- recToLetRec p4 
  let p6 = (etaReduce . replaceCaseBinds . removeRedEqCheck) p5
  let prog = alpha name $
             --removeTyEvidence
             p6 
  env <- getSession
  ws <- liftIO (readIORef ref)
  return (prog, env, nub ws)


compCore :: FilePath -> IO (CoreProgram, HscEnv)
-- | Compile a Core program and apply transformations
compCore fp = runGhc (Just libdir) $ do
    let fname = getExerciseName fp
    (prog,env) <- compCoreSt False fp
    return (prog,env)


appTransf :: (CoreProgram-> Ghc CoreProgram) -> (CoreProgram, HscEnv)  -> Ghc (CoreProgram, HscEnv)
-- | Apply a transformation to a coreprogram and hsc env
appTransf transf (p, env) = do
    setSession env
    p' <- transf p
    env' <- getSession
    return (p',env')

ghcToIO :: Ghc (CoreProgram, HscEnv) -> IO (CoreProgram, HscEnv)
ghcToIO x = runGhc (Just libdir) $ do x

{- compNorm :: ExerciseName -> FilePath -> IO (CoreProgram, HscEnv)
-- | Compile a Core program and apply transformations
compNorm fname fp = runGhc (Just libdir) $ do
    (p',e) <- compCoreSt False fp
    let p = removeModInfo p'
    (p1,e1) <- appTransf repHoles (p,e)
    (p2,e2) <- appTransf (inlineBinds) (p1,e1)
    (p3,e3) <- appTransf (recToLetRec) (p2,e2)
    (p4,e4) <- appTransf (etaExpP) (p3,e3)
    let
        p5 = (alpha fname . floatOutLets . etaReduce . removeRedEqCheck) p4 
        prog = removeTyEvidence 
               p5
        env = e2
    return (prog,env) -}
compNorm :: ExerciseName -> FilePath -> IO (CoreProgram, HscEnv, [Warning])
-- | Compile a Core program and apply transformations
compNorm fname fp = runGhc (Just libdir) $ do
    ref <- liftIO (newIORef [])
    pushLogHookM (writeWarnings ref)
    (p',e) <- compCoreSt False fp
    let p = removeModInfo p'
        p1 = repHoles p
    p2 <- inlineBinds p1
    p3 <- recToLetRec p2 
    let
        p5 = (alpha fname . etaReduce . replaceCaseBinds . removeRedEqCheck) p3
        prog = removeTyEvidence
               p5
    env <- getSession
    warns <- liftIO $ readIORef ref 
    return (prog,env,nub warns)

compFloat :: FilePath -> IO (CoreProgram, HscEnv)
-- | Compile a Core program and apply float transformations
compFloat fp = runGhc (Just libdir) $ do
    let fname = takeWhile (/= '/') fp
    (p',e) <- compCoreSt False fp
    let p = removeModInfo p'
    (p2,e2) <- appTransf (floatOut) (p,e)
    (p3,e3) <- appTransf (etaExpP) (p2,e2)
    let p4 = alpha fname p3
    return (p4,e3)

--- Convienience functions for printing directly    
compC :: FilePath -> IO CoreProgram
-- | Compile to coreprogram, after desugaring pass, before simplifier 
compC fp = runGhc (Just libdir) $ compCoreSt False fp  >>= \(p,_) -> return p

compN :: ExerciseName -> FilePath -> IO (CoreProgram,HscEnv)
compN n fp = do
  (p,e,_) <- compNorm n fp
  return (p,e)

compSt :: ExerciseName -> FilePath -> IO CoreProgram
compSt fn fp = compSimpl fn fp >>= (\(x,_,_) -> return x)

compS :: ExerciseName -> FilePath -> IO (CoreProgram,HscEnv)
compS fname fp = do
  (p,e,_) <- compSimpl fname fp
  return (p,e)


-- Functions to compile wiht plugins 
compWithPlugins :: FilePath -> IO CoreProgram
compWithPlugins fp = runGhc (Just libdir) $ do
    dflags <- setFlags False (holeFlags ++ genFlags ++ simplFlags)
    setSessionDynFlags (gopt_set dflags Opt_DoCoreLinting)
    dflags' <- getSessionDynFlags
    target <- guessTarget fp Nothing
    loadWithPlugins target [StaticPlugin $ PluginWithArgs
            { paArguments = [],
              paPlugin = plugin -- plugin function (imported from MyPlugin)
            }]
    modSum <- getModSummary $ mkModuleName (takeBaseName fp)
    env <- getSession
    pmod <- parseModule modSum
    tmod <- typecheckModule pmod
    dmod <- desugarModule tmod
    names <- getNamesInScope
    let coremod = dm_core_module dmod
    -- continue with simplifier (core2core)
    --coremod' <- liftIO $ core2core env (coremod) -- should depend on which general flags are set
    let coretodo = CoreDoPasses [CoreDesugarOpt] -- just try to do next core pass 
    env <- getSession
    -- typecheck core after simplification
    liftIO $ lintPassResult env coretodo (mg_binds coremod)
    return (removeModInfo (mg_binds coremod))



loadWithPlugins :: GhcMonad m => Target -> [StaticPlugin] -> m SuccessFlag
-- | Load ghc with a list of plugins
loadWithPlugins t the_plugins = do
      -- first unload (like GHCi :load does)
      GHC.setTargets []
      _ <- GHC.load LoadAllTargets
      setTargets [t]

      modifySession $ \hsc_env ->
        let old_plugins = hsc_static_plugins hsc_env
        in hsc_env { hsc_static_plugins = old_plugins ++ the_plugins}

      dflags <- getSessionDynFlags
      setSessionDynFlags dflags { outputFile_ = Nothing }
      load LoadAllTargets

compPureS :: ExerciseName -> FilePath -> IO (CoreProgram, HscEnv, [Warning])
compPureS n fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- setFlags True (holeFlags ++ genFlags ++ simplFlags)
  setSessionDynFlags dflags
  mod <- compileToCoreSimplified fp
  let prog = alpha n (removeModInfo (cm_binds mod))
  return (prog,env,[])

compPureC :: ExerciseName -> FilePath -> IO (CoreProgram, HscEnv, [Warning])
compPureC n fp = do 
  (p,e) <- compCore fp 
  let prog = alpha n (removeModInfo p)
  return (prog,e,[])



-- old functions
-- =========================================
compToTc :: FilePath -> IO TypecheckedSource
-- | Compile to typechecked program (parsing, renaming, typechecking)
compToTc fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- setFlags False (holeFlags  ++ genFlags)
  setSessionDynFlags dflags
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum
  tmod <- typecheckModule pmod
  let tprogram = tm_typechecked_source tmod
  liftIO $ putStrLn $ showGhc dflags tprogram
  return tprogram

compCoreGuts :: FilePath -> Ghc (ModGuts, HscEnv)
compCoreGuts fp = do
  env <- getSession
  dflags <- setFlags False (holeFlags ++ genFlags)
  setSessionDynFlags (gopt_set dflags Opt_DoCoreLinting)
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  mod <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule mod
  tmod <- typecheckModule pmod
  dmod <- desugarModule tmod
  let cmod = coreModule dmod
  env' <- getSession
  return (cmod, env')


compilePrint :: FilePath -> IO ()
-- | Compile to Core, print information from all passes and some additional information
compilePrint file = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let opt_flags = EnumSet.fromList $ [Opt_DeferTypedHoles, Opt_DoEtaReduction, Opt_DoLambdaEtaExpansion]
                         ++ holeFlags
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = opt_flags}
  setSessionDynFlags (gopt_set dflags' Opt_DoCoreLinting)

  target <- guessTarget file Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName file)

  pmod <- parseModule modSum
  let psrc = pm_parsed_source pmod
  -- just for test, error must not be handled like this
  --tmod <- handleSourceError (\e -> err or $ "TYPE ERROR\n" ++ show e) (typecheckModule pmod) 

  tmod <- typecheckModule pmod
  let tsrc = tm_typechecked_source tmod

  let (tcenv, mod) = tm_internals_ tmod
  let tcenv' = tcg_type_env tcenv
  liftIO $ banner "TCENV"

  liftIO $ putStrLn $ showGhc dflags' tcenv'
  liftIO $ putStrLn $ showGhc dflags' (tcg_top_loc tcenv)

  let warnings = tcg_warns tcenv :: Warnings
      pattern_syn = tcg_patsyns tcenv -- not sure what this is
  liftIO $ banner "Warnings"
  liftIO $ print warnings
  -- hur kan det vara no warnings??

  dmod <- desugarModule tmod      -- DesugaredModule
  let core = dm_core_module dmod  -- :: ModGuts   -- let core = coreModule dmod      -- :: ModGuts
      tmod = dm_typechecked_module dmod -- typechecked module for the desugared module
      coreprog = mg_binds core-- mg_binds core :: CoreProgram -- (:: [CoreBind])

  b <- getBindings

  liftIO $ banner "BINDINGS"
  liftIO $ putStrLn $ showGhc dflags' b

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc dflags' ( parsedSource pmod )

  liftIO $ banner "Renamed Module"
  liftIO $ putStrLn $ showGhc dflags' ( tm_renamed_source tmod )

  liftIO $ banner "Typechecked Module"
  liftIO $ putStrLn $ showGhc dflags' ( tm_typechecked_source tmod )

  liftIO $ banner "Typed Toplevel Definitions"
  liftIO $ putStrLn $ showGhc dflags' ( modInfoTyThings (moduleInfo tmod) )

  liftIO $ banner "Typed Toplevel Exports"
  let modInf = modInfoExports (moduleInfo tmod)
  liftIO $ putStrLn $ showGhc dflags' ( modInfoExports (moduleInfo tmod) )

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc dflags' coreprog

  liftIO $ banner "AST Core" -- print Core AST 
  liftIO $ print coreprog

  liftIO $ banner "Lint pass"
  -- test if core-to-core transformations introduced errors 
  let coretodo = CoreDoNothing
  env <- getSession
  liftIO $ lintPassResult env coretodo coreprog

  liftIO $ banner "Print literals" -- print literals (to see hole fit candidates)
  liftIO $ putStrLn $ findLiterals coreprog

  liftIO $ banner "Print hole loc"
  liftIO $ printHoleLoc coreprog

{-
Maybe this gives hints on how to set/unset certain simplificatoins
coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag CoreLiberateCase         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoExitify            = Just Opt_D_dump_exitify
coreDumpFlag CoreDoDemand             = Just Opt_D_dump_stranal
coreDumpFlag CoreDoCpr                = Just Opt_D_dump_cpranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds_preopt
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep
coreDumpFlag CoreOccurAnal            = Just Opt_D_dump_occur_anal

coreDumpFlag CoreAddCallerCcs         = Nothing
coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing
-}
