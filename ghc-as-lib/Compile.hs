{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- disable warnings from unrecognised pragma in model solution files 
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-all #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module Compile where

-- | Main module for compiling to different compilation passes 

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
      LoadHowMuch (LoadAllTargets), TypecheckedModule (..), TypecheckedSource, GhcTc, HsExpr (HsApp, HsUnboundVar), noLocA, GenLocated (L), LHsExpr, DesugaredModule (dm_core_module), ParsedSource, GhcPs, ParsedModule (pm_parsed_source), MapXRec (mapXRec), SrcSpan (RealSrcSpan), SrcSpanAnnA, SrcSpanAnn' (SrcSpanAnn), Target, SuccessFlag, srcLocFile, ParsedMod (parsedSource))
import GHC.Paths (libdir)
import GHC.Driver.Session
    ( DynFlags(..),
      GeneralFlag(..),
      WarningFlag(..),
      gopt,
      gopt_unset,
      wopt_set,
      wopt_unset,
      WarnReason(..) ) 
import GHC.Utils.Outputable 
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

import System.FilePath ( replaceDirectory, takeBaseName )
import Debug.Trace (trace)
import Control.Monad (unless)

import Transform
    ( alpha, normalise, preProcess, removeModInfo, removeTyEvidence ) 
import GHC.Core.Opt.Monad
    ( liftIO,
      CoreToDo(..) ) 
import GHC.Core.Opt.Pipeline (core2core)
import GHC.IO (catchException, catchAny)
import GHC.Utils.Logger (LogAction)
import qualified GHC.Utils.Ppr as Pretty
import System.IO 


import Data.Function (on)
import Data.List ( nub , nubBy ) 
import Data.Data (Data)
import Data.IORef ( newIORef, readIORef, IORef, modifyIORef ) 
import Utils 
import Warning 
import Data.Generics.Uniplate.Data
import Annotation
import GHC.Data.Bag (bagToList)

import Splint (plugin )
import GHC.Driver.Plugins (PluginWithArgs(..), StaticPlugin(..))

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
   let 
       dflags = Prelude.foldl gopt_unset df unsetGenFlags
       dflags1 = Prelude.foldl wopt_unset dflags unsetWarnFlags
       dflags2 = Prelude.foldl wopt_set dflags1 setWarnFlags
       gflags = if b then EnumSet.toList (generalFlags dflags2) ++ flags else flags
       dflags3 = dflags2 {refLevelHoleFits = Just 2,
                          maxValidHoleFits = Just 8,
                          maxRefHoleFits   = Just 10,
                          generalFlags = EnumSet.fromList gflags}
   setSessionDynFlags dflags3 


data CompInfo = CompInfo {
    core :: CoreProgram,
    parsed :: ParsedSource,
    warns  :: [Warning]
}

loadWithPlugins :: GhcMonad m => DynFlags -> Target -> [StaticPlugin] -> m SuccessFlag
-- | Load ghc with a list of plugins
loadWithPlugins dflags t the_plugins = do
      -- first unload (like GHCi :load does)
      GHC.setTargets []
      _ <- GHC.load LoadAllTargets
      setTargets [t]
      modifySession $ \hsc_env ->
        let old_plugins = hsc_static_plugins hsc_env
        in hsc_env { hsc_static_plugins = old_plugins ++ the_plugins}
      setSessionDynFlags dflags { outputFile_ = Nothing }
      load LoadAllTargets

toDesugar' :: Bool -> [GeneralFlag] -> FilePath -> Ghc (ModGuts, ParsedSource, IORef [Warning])
-- | Compile a file to the desugar pass + simple optimiser and return Modguts and warnings
toDesugar' setdefaultFlags flags fp = do
  env <- getSession
  setFlags setdefaultFlags flags 
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  target <- guessTarget fp Nothing
  dflags <- getSessionDynFlags
  --liftIO $ print dflags 
  loadWithPlugins dflags target [StaticPlugin $ PluginWithArgs
            { paArguments = [],
              paPlugin = plugin -- hlint plugin
            }] 
  
  modSum <- getModSummary $ mkModuleName (takeBaseName fp) 
  pmod <- parseModule modSum 
  tmod <- typecheckModule pmod 
  let tprog = attachNote (tm_typechecked_source tmod)
  dmod <- desugarModule (tmod {tm_typechecked_source = tprog})
  let modguts = coreModule dmod 
  cprog <- liftIO $ preProcess (mg_binds modguts) -- apply preprocessing transformations
  let mg = modguts {mg_binds = cprog} 
  return (mg, (pm_parsed_source pmod), ref)

toSimplify :: FilePath -> Ghc (CoreProgram, ParsedSource, IORef [Warning])
-- | Replace holes and run simplifier 
toSimplify fp = do 
  (mgCore,psrc,ref) <- toDesugar' True (holeFlags ++ genFlags ++ simplFlags) fp 
  env <- getSession
  mgSimpl <- liftIO $ core2core env (mgCore {mg_binds = (mg_binds mgCore)})
  return (mg_binds mgSimpl,psrc,ref)

toDesugar :: FilePath -> Ghc (CoreProgram, ParsedSource, IORef [Warning])
-- | Desugar and return coreprogram and warnings  
toDesugar fp = 
    (toDesugar' False (holeFlags ++ genFlags) fp) >>= \(mg,psrc,ref) -> return (mg_binds mg,psrc,ref)


compSimplNormalised :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile a file to normalised Core with simplifier
compSimplNormalised name fp = runGhc (Just libdir) $ do 
  (prog,psrc,ref) <- toSimplify fp 
  prog' <- liftIO $ normalise name prog 
  ws <- liftIO (readIORef ref)
  return $ CompInfo (removeTyEvidence prog') psrc (nubBy uniqWarns ws)

compDesNormalised :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile a file to normalised Core without simplifier
compDesNormalised name fp = runGhc (Just libdir) $ do 
  (prog,psrc,ref) <- toDesugar fp 
  prog' <- liftIO $ normalise name prog
  ws <- liftIO (readIORef ref)
  return $ CompInfo (removeTyEvidence prog') psrc (nubBy uniqWarns ws)

compSimpl :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile to simplified core directly, return renamed program and warnings
compSimpl _ fp = runGhc (Just libdir) $ do
  (prog,psrc,ref) <- toSimplify fp
  ws <- liftIO $ readIORef ref  
  return $ CompInfo (removeTyEvidence prog) psrc (nubBy uniqWarns ws)

compDes :: ExerciseName -> FilePath -> IO CompInfo
-- | Compile to desugared core directly, return renamed program and warnings
compDes _ fp = runGhc (Just libdir) $ do
  (prog,psrc,ref) <- toDesugar fp 
  ws <- liftIO $ readIORef ref
  return $ CompInfo (removeTyEvidence prog) psrc (nubBy uniqWarns ws)


--- For testing purposes 

compString :: String -> ExerciseName -> IO CompInfo
compString input exercise = do 
  rules <- readFile "Rules.hs"
  let inputstr = "module Temp where\n" `nl` input `nl` rules
  handle <- openFile "studentfiles/Temp.hs" WriteMode
  hPutStrLn handle inputstr
  hFlush handle
  hClose handle
  compSimplNormalised exercise "./studentfiles/Temp.hs"


compTestNorm :: (FilePath -> Ghc (CoreProgram, ParsedSource, IORef [Warning]))
                -> ExerciseName -> FilePath -> IO (CoreProgram, HscEnv)
-- | Compile with given function, return normalised program and environment
compTestNorm f n fp = runGhc (Just libdir) $ do
  (core, p, w) <- f fp 
  prog <- liftIO $ normalise n core  
  env <- getSession 
  return (prog,env)


typeCheckCore :: CoreProgram -> HscEnv -> IO ()
-- | Use Core Linter to check for problems
typeCheckCore coreprog env = do
   let coretodo = CoreDoPasses [CoreDesugar, CoreDesugarOpt, CoreTidy, CorePrep]
       dflags = hsc_dflags env
   unless (gopt Opt_DoCoreLinting dflags) $ error "CoreLinting flag must be set"
   liftIO $ lintPassResult env coretodo (coreprog)

