{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- disable warnings from unrecognised pragma in model solution files 
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
      CoreModule (cm_binds, cm_types), pprVarSig, compileToCoreModule, SrcLoc, getContext, getRealSrcSpan, lookupName, TypecheckedSource, gopt, ModSummary (ModSummary), ParsedModule (ParsedModule))

--import GHC.Show
import GHC.Paths (libdir)
import GHC.Data.Bag ()

import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut, gopt_set, optimisationFlags, dopt_set)
import GHC.Driver.Flags ( DumpFlag(..), GeneralFlag (..))
import GHC.Utils.Outputable ( Outputable(..), showSDocUnsafe)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Unit.Module.ModGuts (mg_binds, ModGuts (mg_module, mg_binds))
import GHC.Core ( Bind(..), Expr(..), CoreProgram, Alt(..), AltCon(..), CoreBind(..), CoreExpr, CoreBndr)
import GHC.Driver.Ppr (showPpr)
import GHC.CoreToStg (coreToStg)
import GHC.Types.SourceError ( handleSourceError, SourceError (SourceError))
import GHC.Types.Error (getErrorMessages, MsgEnvelope)
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
import GHC.Driver.Env (HscEnv(hsc_plugins, hsc_static_plugins, hsc_dflags, hsc_logger, HscEnv, hsc_IC, hsc_NC))
import GHC.Driver.Monad (modifySession, Ghc, putMsgM, liftGhcT, withSession)
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
import Control.Monad (when)

import Similar
import Instance 
import Transform (etaReduce, alpha, removeModInfo, replaceHoles, rewriteRec, normalise, normalise')
import Utils 
import GHC.Core.Opt.Monad (CoreToDo (..), getRuleBase, CoreM)
import GHC.Core.Opt.Pipeline (core2core)
import GHC.Iface.Ext.Utils (getNameScope)
import GHC.Core.Opt.Simplify.Env (getSimplRules)
import GHC.Driver.Plugins 
import MyPlugin (plugin, install)
import GHC.Plugins (mkModuleNameFS, ModGuts (ModGuts), Unique, mkLocalVar, IdDetails (VanillaId), mkInternalName, mkOccName, mkGeneralSrcSpan, vanillaIdInfo, mkInScopeSet, mkUniqSet, MonadUnique (getUniqueM), setVarType, extendInScopeSet, unsafeGetFreshLocalUnique, moduleEnvElts, InScopeSet, getInScopeVars, getUniqSet, eltsUFM, mkGlobalVar, isFunTy, Uniquable (getUnique), idInfo, exprType)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import qualified GHC.Types.Name.Occurrence as Occ
import Data.Generics.Uniplate.Data
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)
import GHC.Runtime.Context (extendInteractiveContextWithIds, icInScopeTTs)
import Data.Data (Data)
import Data.IORef (readIORef)
import GHC.Types.Name.Cache (NameCache(..))



{- DynFlags plugin options
 pluginModNames        :: [ModuleName],
    -- ^ the @-fplugin@ flags given on the command line, in *reverse*
    -- order that they're specified on the command line.
  pluginModNameOpts     :: [(ModuleName,String)],
  frontendPluginOpts    :: [String],
    -- ^ the @-ffrontend-opt@ flags given on the command line, in *reverse*
    -- order that they're specified on the command line.

  externalPluginSpecs   :: [ExternalPluginSpec],

-}
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
            --Opt_FloatIn
             --,Opt_LiberateCase
             Opt_DoLambdaEtaExpansion
             --,Opt_CaseMerge
             --,Opt_EnableRewriteRules
             ]

genFlags :: [GeneralFlag]
genFlags = [Opt_DeferTypedHoles
           ,Opt_DoEtaReduction
           ,Opt_DoLambdaEtaExpansion]

compExpr :: String -> IO ()
compExpr expr = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  e <- parseExpr expr
  undefined 

setFlags :: Bool -> [GeneralFlag] -> Ghc DynFlags 
setFlags b flags = do 
   dflags <- getSessionDynFlags 
   let dflags' = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags) 
   let flags' = if b then EnumSet.toList (generalFlags dflags) ++ flags else flags   
       dflags'  = dflags {refLevelHoleFits = Just 2,
                          maxValidHoleFits = Just 8,
                          maxRefHoleFits   = Just 15,
                          generalFlags = EnumSet.fromList flags'} 
   return dflags'   

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


compToFile :: (FilePath -> IO CoreProgram) -> FilePath -> IO ()
-- | Compile with given compile function and write to file 
compToFile compile file = do
  coreprog <- compile file 
  let file_out = replaceDirectory file "./out/"
  liftIO $ writeFile file_out $ show coreprog


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


compSimpl :: Bool -> FilePath -> IO CoreProgram
-- | Compile coreprogram, after simplifier pass 
compSimpl use_defaultflags file = runGhc (Just libdir) $ do
  env <- getSession 
  dflags <- setFlags use_defaultflags (holeFlags ++ genFlags) -- set dynflags 
  setSessionDynFlags dflags 
  simpl <- compileToCoreSimplified file
  return (cm_binds simpl)

compSimplAnal :: FilePath -> IO CoreProgram
-- | Compile with simplifier and see simplifier stats 
compSimplAnal fp =  runGhc (Just libdir) $ do
  env <- getSession 
  dflags <- setFlags False (holeFlags ++ genFlags)-- set dynflags 
  setSessionDynFlags (dopt_set dflags Opt_D_dump_simpl_stats) -- set flag for simplification stats  
  simpl <- compileToCoreSimplified fp
  return (cm_binds simpl)

compSetSimplPass :: FilePath -> IO CoreProgram 
-- | Try to set specific simplifier passes with general flags
-- which does not seem to work 
compSetSimplPass fp = runGhc (Just libdir) $ do
    dflags <- setFlags False (holeFlags ++ genFlags ++ simplFlags) 
    --let dflags' = dflags {pluginModNames = [mkModuleNameFS $ fsLit "Plugins1.MyPlugin"]}
    setSessionDynFlags (gopt_set dflags Opt_DoCoreLinting) 
    dflags' <- getSessionDynFlags
    target <- guessTarget fp Nothing
    setTargets [target]
    load LoadAllTargets
    --loadWithPlugins target [StaticPlugin $ PluginWithArgs
    --        { paArguments = [],
    --          paPlugin = plugin
    --        }]
    modSum <- getModSummary $ mkModuleName (takeBaseName fp) 
    env <- getSession 
    pmod <- parseModule modSum 
    tmod <- typecheckModule pmod 
    dmod <- desugarModule tmod 
    names <- getNamesInScope 
    let coremod = dm_core_module dmod 
    --liftIO $ putStrLn $ showSDocUnsafe $ ppr names -- no names in scope?? prints nohing 
    let mguts     = dm_core_module dmod  
        --coreprog  = mg_binds mguts 
    (mguts',env') <- repHoles mguts 
    liftIO $ print "INSCOPE VARIABLES"
    liftIO $ print $ interactiveInScope $ hsc_IC env' 
    --n <- liftIO $ readIORef $ hsc_NC env  
    --liftIO $ putStr $ showSDocUnsafe $ ppr $ moduleEnvElts (nsNames n) -- this is like the prelude, huge output
        --mguts'    = mguts {mg_binds = coreprog'} 
    --coremod' <- liftIO $ core2core env (mguts') -- should depend on which general flags are set
    let coretodo = CoreDoPasses [CoreDesugarOpt] -- just try to do next core pass 
    --env <- getSession 
    -- typecheck core after simplification
    --liftIO $ print $ removeModInfo $ mg_binds mguts' 
    liftIO $ lintPassResult env coretodo (mg_binds mguts')
    return $ removeModInfo $ mg_binds mguts'
  
  
--loadWithPlugins :: GhcMonad m => [StaticPlugin] -> m SuccessFlag
loadWithPlugins t the_plugins = do
      -- first unload (like GHCi :load does)
      GHC.setTargets []
      _ <- GHC.load LoadAllTargets

      --target <- guessTarget "MyPlugin.hs" Nothing
      setTargets [t] 

      modifySession $ \hsc_env ->
        let old_plugins = hsc_static_plugins hsc_env
        in hsc_env { hsc_static_plugins = old_plugins ++ the_plugins} 

      dflags <- getSessionDynFlags
      setSessionDynFlags dflags { outputFile_ = Nothing }
      load LoadAllTargets


compCore :: Bool -> FilePath -> IO CoreProgram
-- | Compile to coreprogram, after desugaring pass, before simplifier 
compCore b fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- setFlags b (holeFlags ++ genFlags)
  setSessionDynFlags dflags
  coremod <- compileToCoreModule fp
  return $ cm_binds coremod 


compNormNoRename :: FilePath -> IO CoreProgram 
compNormNoRename fp = runGhc (Just libdir) $ do
    dflags <- setFlags False (holeFlags ++ genFlags ++ simplFlags) 
    setSessionDynFlags (gopt_set dflags Opt_DoCoreLinting) 
    dflags' <- getSessionDynFlags
    coremod' <- liftIO $ compCore False fp 
    let fname = takeWhile (/= '/') fp                        -- but seems to do a lot of simplifications anyway 
        normalised_prog = normalise' fname coremod'
        coretodo = CoreDoPasses [CoreDesugarOpt, CoreDesugar] -- just try to do next core pass 
    env <- getSession 
    -- typecheck core after transformation (except renaming transformations)
    -- since GHC's Subst environment is not updated when renaming 
    liftIO $ lintPassResult env coretodo (normalised_prog)
    return normalised_prog


compNorm :: FilePath -> IO CoreProgram 
compNorm fp = do  
    p <- compCore False fp 
    let fname = takeWhile (/= '/') fp 
    return $ normalise fname p 

  
compileAndNormalise :: ExerciseName -> (FilePath -> IO CoreProgram) -> FilePath -> IO [CoreProgram]
-- | Compile file and apply transformations, return program from each step  
compileAndNormalise name compile fp = do
  cp <- compile fp 
  let rcp  = removeModInfo cp   -- module info removed
  let ecp  = etaReduce rcp      -- manual eta reduce 
  let ecp' = replaceHoles ecp 
  let rrp  = rewriteRec name ecp' 
  let acp  = alpha name ecp'    -- alpha renamed, needs to know the name of the exercise for desugar (that we dont want to rename)
  return [cp,rcp,ecp',rrp,acp]  -- return coreprogram of every transformation step 





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

repHoles :: ModGuts -> Ghc (ModGuts, HscEnv)
repHoles mg@ModGuts {mg_binds = prog} = do
  env <- getSession 
  let ic = hsc_IC env 
  let inscopeVars = interactiveInScope ic 
  (prog',inscopeVars') <- replaceHoles (mkInScopeSet $ mkUniqSet inscopeVars) prog
  let ic' = extendInteractiveContextWithIds ic (eltsUFM $ getUniqSet $ getInScopeVars inscopeVars')
  let env' = env {hsc_IC = ic'}
  modifySession $ const env'
  return $ (mg {mg_binds = prog'},env')
        where replaceHoles :: InScopeSet -> CoreProgram -> Ghc (CoreProgram,InScopeSet)
              -- | Replace expressions representing holes with hole variables of the same type 
              replaceHoles is cs = runStateT (tr cs) is
                where tr :: CoreProgram -> StateT InScopeSet Ghc CoreProgram
                      tr = transformBiM $ \case
                        c@(Case e v t _) | isHoleExpr c -> 
                                                           let id = fromJust (getTypErr e)
                                                           in newHoleVar t >>= \i -> return $ Var i
                                         | otherwise -> return c
                        e -> return e

rewriteCase :: Var -> Expr Var -> Expr Var 
rewriteCase id (Case e v t _) = extract e t id 

extract :: Expr Var -> Type -> Var -> Expr Var 
extract (App e (Var te)) t v | getOccString te == "typError" = App (App e (Var v)) (Type $ ft_arg t)

newHoleVar :: Type -> StateT InScopeSet Ghc Var
newHoleVar t = do
    is <- get
    --let is = mkInScopeSet $ mkUniqSet vars
    --uq <- lift getUniqueM
    let uq = unsafeGetFreshLocalUnique is
    let is' = case t of 
          (TyVarTy tv) -> extendInScopeSet is' tv 
          typ          -> is 
    --let uq = getUnique v 
    let name = "hole"
        id   = setIdExported $ setVarType (makeVar uq t name) t  
        is'' = extendInScopeSet is' id
    put is''
    return id


-- check function newLocal :: FastString -> ScaledType -> UniqSM Id from GHC.Types.Id.Make 

{- newHoleVar :: Id -> Type -> Ctx Id
newHoleVar id t = do
    let uq@(c,i) = unpkUnique $ getUnique id
    j <- freshHNum
    let name = "hole_"++ show j
    let id' = setVarType (makeVar id name) t
    modify $ \s -> s {env = Map.insert id' id' (env s), freshHNum = j+1} -- update map 
    return id' -}

getTypErr :: Expr Var -> Maybe Var
getTypErr e = head [Just v | (Var v) <- universe e, getOccString v == "typeError"]

makeVar :: Unique -> Type -> String -> Var 
makeVar uq t n = mkGlobalVar id_det name typ id_inf
        where id_det = VanillaId 
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = if isFunTy t then ft_mult t else t 
              typ    = t
              id_inf = vanillaIdInfo 
