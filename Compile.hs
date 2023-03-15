{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- disable warnings from unrecognised pragma in model solution files 
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

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
      TypecheckedMod(moduleInfo),
      TypecheckedModule(tm_typechecked_source, tm_renamed_source, TypecheckedModule, tm_internals_),
      LoadHowMuch(LoadAllTargets),
      GhcMonad(getSession, setSession),
      DynFlags(..),
      SuccessFlag,
      DesugaredModule (dm_core_module, dm_typechecked_module),
      compileToCoreSimplified,
      parseExpr,
      CoreModule (cm_binds, cm_types), pprVarSig, compileToCoreModule, SrcLoc, getContext, getRealSrcSpan, lookupName, TypecheckedSource, gopt)

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
import GHC.Types.Var (Var(..), TyCoVarBinder(..), VarBndr(..), ArgFlag(..), AnonArgFlag, TyCoVar, Specificity(..))
import GHC.Types.Literal (Literal(..), LitNumType, pprLiteral)
import GHC.Types.Basic (FunctionOrData (IsFunction, IsData))
import GHC.Types.Tickish ( CoreTickish )
import GHC.Core.TyCon (TyCon(..))
import GHC.Core.TyCo.Rep (Type(..), CoercionR, TyLit(..), TyCoBinder)
import GHC.Types.Name (Name(..), isHoleName, nameStableString, OccName, getSrcLoc, NamedThing (getName), pprOccName, pprDefinedAt, getOccString, nameUnique, HasOccName (occName), isDataConName, isTyConName, isTyVarName, isSystemName)
import GHC.Tc.Types (TcGblEnv(..))

import GHC.Tc.Errors.Hole ()
import GHC.Tc.Errors.Hole.FitTypes ()
import GHC.Driver.Env (HscEnv(hsc_plugins, hsc_static_plugins, hsc_dflags))
import GHC.Driver.Monad (modifySession, Ghc, putMsgM, liftGhcT)
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
import GHC.Data.FastString (fsLit)

import System.FilePath ( replaceDirectory, takeBaseName )
import Debug.Trace (trace)
import System.Posix.Internals (puts)
import Control.Monad (when)

import Similar
import Instance 
import Transform (applymany, etaRed, alpha, removeModInfo, replaceHoles)
import Utils 


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

setFlags :: Bool -> Ghc DynFlags 
setFlags b = do 
   dflags <- getSessionDynFlags 
   let dflags' = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags) 
   let flags = if b then EnumSet.toList (generalFlags dflags) ++ addFlags else addFlags  
       addFlags = holeFlags ++ genFlags
       dflags'  = dflags {refLevelHoleFits = Just 2,
                          maxValidHoleFits = Just 8,
                          maxRefHoleFits   = Just 15,
                          generalFlags = EnumSet.fromList flags} 
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
  setSessionDynFlags dflags' 

  target <- guessTarget file Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName file)

  pmod <- parseModule modSum      -- ModuleSummar
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
  liftIO $ putStrLn $ showGhc dflags' ( modInfoExports (moduleInfo tmod) )

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc dflags' coreprog

  liftIO $ banner "AST Core" -- print Core AST 
  liftIO $ print coreprog

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
  dflags <- setFlags False 
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
  dflags <- setFlags use_defaultflags -- set dynflags 
  setSessionDynFlags dflags 
  simpl <- compileToCoreSimplified file
  return (cm_binds simpl)

compSimplAnal :: FilePath -> IO CoreProgram
-- | Compile with simplifier and see simplifier stats 
compSimplAnal fp =  runGhc (Just libdir) $ do
  env <- getSession 
  dflags <- setFlags False -- set dynflags 
  setSessionDynFlags (dopt_set dflags Opt_D_dump_simpl_stats) -- set flag for simplification stats  
  simpl <- compileToCoreSimplified fp
  return (cm_binds simpl)

  

compCore :: Bool -> FilePath -> IO CoreProgram
-- | Compile to coreprogram, after desugaring pass, before simplifier 
compCore b fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- setFlags b 
  setSessionDynFlags dflags
  coremod <- compileToCoreModule fp
  return (cm_binds coremod)

compileAndNormalise :: ExerciseName -> (FilePath -> IO CoreProgram) -> FilePath -> IO [CoreProgram]
-- | Compile file and apply transformations, return program from each step  
compileAndNormalise name compile fp = do
  cp <- compile fp 
  let rcp = removeModInfo cp  -- module info removed
  let ecp = applymany etaRed rcp -- manual eta reduce 
  let ecp' = replaceHoles ecp 
  let acp = alpha name ecp'    -- alpha renamed, needs to know the name of the exercise for desugar (that we dont want to rename)
  return [cp,rcp,ecp',acp]     -- return coreprogram of every transformation step 


compNormalisedPrint :: ExerciseName -> (FilePath -> IO CoreProgram) -> FilePath -> IO ()
compNormalisedPrint name compile fp = do
    [p,rp,ep,ap] <- compileAndNormalise name compile fp 
    banner "Compiled program" >> print p 
    banner "Remove mods" >> print rp 
    banner "Eta-reduce" >> print ep 
    banner "Alpha-rename" >> print ap 


