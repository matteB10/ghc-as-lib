module Compile.Compile where

-- | Main module for compiling programs to different compilation passes 

import Prelude hiding (span)

import GHC
    ( compileToCoreModule,
      compileToCoreSimplified,
      desugarModule,
      getModSummary,
      guessTarget,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      load,
      getSessionDynFlags,
      pushLogHookM,
      gopt,
      mkModuleName,
      coreModule,
      cm_binds,
      NoExtField (..),
      HsExpr (..),
      reLocA,
      realSrcSpan,
      la2r,
      defaultErrorHandler,
      LoadHowMuch (LoadAllTargets), TypecheckedModule (..), TypecheckedSource, GhcTc, HsExpr (HsApp, HsUnboundVar), noLocA, GenLocated (L), LHsExpr, DesugaredModule (dm_core_module), ParsedSource, GhcPs, ParsedModule (pm_parsed_source), MapXRec (mapXRec), SrcSpan (RealSrcSpan), SrcSpanAnnA, SrcSpanAnn' (SrcSpanAnn), Target, SuccessFlag, srcLocFile, ParsedMod (parsedSource), ModSummary)
import GHC.Paths (libdir)
import GHC.Driver.Session
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Unit.Module.ModGuts ( ModGuts(mg_binds) ) 
import GHC.Core ( CoreProgram, Bind(..), Expr(..), Alt(..), AltCon(..), CoreBind(..), CoreExpr, CoreBndr)
import GHC.Types.Error
    ( SDoc, Severity(SevError, SevFatal, SevWarning) ) 
import GHC.Types.Tickish ( GenTickish(..), CoreTickish)
import GHC.Core.Lint ( lintPassResult )
import GHC.Driver.Env ( HscEnv(..) ) 
import GHC.Driver.Monad
    ( liftIO,
      getSessionDynFlags,
      pushLogHookM,
      modifySession,
      Ghc,
      GhcMonad(getSession) ) 
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Unit.Module.Warnings (Warnings (..), pprWarningTxtForMsg)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.SrcLoc (SrcSpan, mkRealSrcLoc, mkRealSrcSpan, RealSrcSpan, pprUserRealSpan, isGeneratedSrcSpan)
import GHC.Core.Opt.Monad
    ( liftIO,
      CoreToDo(..) ) 
import GHC.Core.Opt.Pipeline (core2core)
import GHC.IO (catchException, catchAny)
import GHC.Utils.Logger (LogAction)
import qualified GHC.Utils.Ppr as Pretty

import System.IO 
import Data.List (delete, nubBy) 
import Data.Data (Data)
import Data.IORef ( newIORef, readIORef, IORef, modifyIORef ) 
import GHC.Driver.Plugins (PluginWithArgs(..), StaticPlugin(..))
import GHC.Types.Id (Var)
import GHC.Data.Bag (bagToList)
import GHC.LanguageExtensions (Extension(ExtendedDefaultRules, MonomorphismRestriction))

import Data.Generics.Uniplate.Data
import System.FilePath ( replaceDirectory, takeBaseName )
import Splint (plugin)
import Data.Map ( Map )
import qualified Data.Map as Map 
import Utils.Utils 
import Compile.Warning ( Warning, uniqWarns, writeWarnings ) 
import Transform.Transform

------ GHC settings 
extFlags :: [Extension]
-- | Extension flags to enable
extFlags = [ExtendedDefaultRules] 

unsetExtFlags :: [Extension]
-- | Extension flags to disable
unsetExtFlags = [MonomorphismRestriction]

holeFlags :: [GeneralFlag]
-- | General flags concerning typed holes 
holeFlags =
  [ Opt_ShowValidHoleFits,
    Opt_ShowHoleConstraints,
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
              Opt_DoLambdaEtaExpansion,
              Opt_EnableRewriteRules
             ]

genFlags :: [GeneralFlag]
-- | List of general flags to enable
genFlags = [
            Opt_DoCoreLinting
           ,Opt_DeferTypedHoles
           ,Opt_DeferTypeErrors
           ,Opt_DeferDiagnostics
           ,Opt_InfoTableMap
           ,Opt_AutoLinkPackages
           ]

unsetGenFlags :: [GeneralFlag]
-- | List of general flags to disable
unsetGenFlags = [Opt_KeepHiFiles
               , Opt_KeepOFiles]

setWarnFlags :: [WarningFlag]
-- | List of warning flags to enable
setWarnFlags = [Opt_WarnOverlappingPatterns
              , Opt_WarnIncompletePatterns
              , Opt_WarnTypedHoles
              ]

unsetWarnFlags :: [WarningFlag]
-- | List of warning flags to disable
unsetWarnFlags = [
                 Opt_WarnUnrecognisedPragmas,
                 Opt_WarnInlineRuleShadowing
                 ]

setFlags :: Bool -> [GeneralFlag] -> Ghc ()
-- | Set dynamic flags 
setFlags b flags = do
   df <- getSessionDynFlags
   let gflags  = if b then EnumSet.toList (generalFlags df) ++ flags else flags
       dflags = (opt_set gopt_unset unsetGenFlags
              . opt_set wopt_unset unsetWarnFlags
              . opt_set wopt_set setWarnFlags
              . opt_set eopt_set extFlags
              . opt_set eopt_unset unsetExtFlags
              . opt_set gopt_set gflags) df 

       {- to update DynFlags directly
       dflags' = dflags {refLevelHoleFits = Just 2,
                         maxValidHoleFits = Just 8,
                         maxRefHoleFits   = Just 10}
       -}
          
   setSessionDynFlags dflags

opt_set :: (DynFlags -> a -> DynFlags) -> [a] -> DynFlags -> DynFlags
opt_set f flags df = Prelude.foldl f df flags 

eopt_unset :: DynFlags -> Extension -> DynFlags
eopt_unset df ext = df {extensionFlags = EnumSet.delete ext (extensionFlags df)}

eopt_set :: DynFlags -> Extension -> DynFlags
eopt_set df ext = df {extensionFlags = EnumSet.insert ext (extensionFlags df)}

data CompInfo = CompInfo {
  -- compilation information used in feedback generation
    core     :: CoreProgram,
    parsed   :: ParsedSource,
    warns    :: [Warning],
    names    :: Map Var Var, 
    exercise :: ExerciseName
}

loadWithPlugins :: GhcMonad m => DynFlags -> Target -> [StaticPlugin] -> m SuccessFlag
-- | Load ghc with a list of plugins
loadWithPlugins dflags t plugins = do
      -- first unload (like GHCi :load does)
      GHC.setTargets []
      _ <- GHC.load LoadAllTargets
      setTargets [t]
      modifySession $ \hsc_env ->
        let old_plugins = hsc_static_plugins hsc_env
        in hsc_env { hsc_static_plugins = old_plugins ++ plugins} -- set new plugins
      setSessionDynFlags dflags { outputFile_ = Nothing }
      load LoadAllTargets

parseExerciseTypSig :: FilePath -> ExerciseName -> IO String 
-- | Parse exercise type signature from a model file 
parseExerciseTypSig fp name = runGhc (Just libdir) $ do 
    initEnv True (holeFlags ++ genFlags) fp 
    modSum <- getModSummary $ mkModuleName (takeBaseName fp) 
    pmod <- parseModule modSum 
    let sig = getSig name (pm_parsed_source pmod)
    return (showGhcUnsafe sig)

checkTypeSig :: FilePath -> String -> IO Bool
-- | Check type sig before continuing compilation
checkTypeSig fp ename = runGhc (Just libdir) $ do 
    initEnv True (holeFlags ++ genFlags) fp 
    modSum <- getModSummary $ mkModuleName (takeBaseName fp) 
    pmod <- parseModule modSum 
    return $ hasTypSig ename (pm_parsed_source pmod)
    

initEnv :: Bool -> [GeneralFlag] -> FilePath -> Ghc (IORef [Warning])
initEnv setdefaultFlags flags fp = do 
  env <- getSession
  setFlags setdefaultFlags flags 
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  target <- guessTarget fp Nothing
  dflags <- getSessionDynFlags
  loadWithPlugins dflags target [StaticPlugin $ PluginWithArgs
            { paArguments = [],
              paPlugin = plugin -- hlint plugin
            }] 
  return ref 

toDesugar :: Bool -> [GeneralFlag] -> FilePath -> Ghc (ModGuts, ParsedSource, IORef [Warning])
-- | Compile a file to the desugar pass + simple optimiser 
toDesugar setdefaultFlags flags fp = do
  ref <- initEnv setdefaultFlags flags fp 
  modSum <- getModSummary $ mkModuleName (takeBaseName fp) 
  pmod <- parseModule modSum 
  tmod <- typecheckModule pmod 
  dmod <- desugarModule tmod 
  let modguts = coreModule dmod 
  return (modguts, (pm_parsed_source pmod), ref)

toSimplify :: FilePath -> Ghc (CoreProgram, ParsedSource, IORef [Warning])
-- | Apply preprocessing transformations (replace holes and pattern errors)
--   and run simplifier 
toSimplify fp = do 
  (mgCore,psrc,ref) <- toDesugar True (holeFlags ++ genFlags ++ simplFlags) fp 
  env <- getSession
  prog <- liftIO $ preProcess (mg_binds mgCore) 
  mgSimpl <- liftIO $ core2core env (mgCore {mg_binds = prog})
  return (mg_binds mgSimpl,psrc,ref)


-- compile and normalise 
compile :: ExerciseName -> FilePath -> IO CompInfo 
-- | Compile and apply normalisation, removing types as a final step
compile n fp = compile' n fp >>= 
              \x -> return $ x {core = removeTyEvidence (core x)}


compile' :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile a file to normalised Core with simplifier
--   without removing types 
compile' name fp = defaultErrorHandler 
                   defaultFatalMessager 
                   defaultFlushOut $ 
                   runGhc (Just libdir) $ do 
  (prog,psrc,wref) <- toSimplify fp 
  (normprog,names) <- liftIO $ normalise name prog 
  ws <- liftIO (readIORef wref)
  return $ CompInfo normprog psrc (nubBy uniqWarns ws) names name 


--- For testing desugar pass

compileDesugar :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile a file to normalised Core without simplifier
compileDesugar name fp = defaultErrorHandler 
                         defaultFatalMessager 
                         defaultFlushOut $ 
                         runGhc (Just libdir) $ do 
  (mg,psrc,ref) <- toDesugar False (holeFlags ++ genFlags) fp 
  (prog, names) <- liftIO $ normalise name =<< preProcess (mg_binds mg)
  ws <- liftIO (readIORef ref)
  return $ CompInfo (removeTyEvidence prog) psrc (nubBy uniqWarns ws) names name 

-- To run the CoreLinter 

compTestNorm :: ExerciseName -> FilePath -> IO (CoreProgram,HscEnv)
-- | Compile with given function, return normalised program and environment
compTestNorm n fp = runGhc (Just libdir) $ do
  initEnv True (holeFlags ++ genFlags ++ simplFlags) fp 
  res <- liftIO $ compile' n fp  
  env <- getSession 
  return (core res,env)
