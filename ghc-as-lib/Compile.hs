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
      LoadHowMuch (LoadAllTargets))
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
import GHC.Types.Tickish ( CoreTickish )
import GHC.Core.Lint ( lintPassResult )
import GHC.Driver.Env ( HscEnv(hsc_dflags) ) 
import GHC.Driver.Monad
    ( liftIO,
      getSessionDynFlags,
      pushLogHookM,
      Ghc,
      GhcMonad(getSession) ) 
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Unit.Module.Warnings (Warnings (..), pprWarningTxtForMsg)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.SrcLoc (SrcSpan)

import System.FilePath ( replaceDirectory, takeBaseName )
import Debug.Trace (trace)
import Control.Monad (unless)

import Transform
    ( alpha, normalise, preProcess, removeModInfo, removeTyEvidence ) 
import Utils ( ExerciseName )
import GHC.Core.Opt.Monad
    ( liftIO,
      CoreToDo(..) ) 
import GHC.Core.Opt.Pipeline (core2core)
import GHC.IO (catchException, catchAny)
import GHC.Utils.Logger (LogAction)
import qualified GHC.Utils.Ppr as Pretty
import System.IO 


import Data.Function (on)
import Data.List ( nub ) 
import Data.Data (Data)
import Data.IORef ( newIORef, readIORef, IORef, modifyIORef ) 
import Utils 
import Warning 

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
           ,Opt_DoCoreLinting
           ,Opt_DeferDiagnostics
           ,Opt_InfoTableMap
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
              , Opt_WarnRedundantConstraints
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

toDesugar' :: Bool -> [GeneralFlag] -> FilePath -> Ghc (ModGuts, IORef [Warning])
-- | Compile a file to the desugar pass + simple optimiser and return Modguts and warnings
toDesugar' setdefaultFlags flags fp = do
  env <- getSession
  setFlags setdefaultFlags flags 
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  dmod <- desugarModule =<< typecheckModule =<< parseModule modSum 
  let modguts = coreModule dmod 
  cprog <- liftIO $ preProcess (mg_binds modguts) -- apply preprocessing transformations
  let mg = modguts {mg_binds = cprog} 
  return (mg,ref)

toSimplify :: FilePath -> Ghc (CoreProgram, IORef [Warning])
-- | Replace holes and run simplifier 
toSimplify fp = do 
  (mgCore,ref) <- toDesugar' True (holeFlags ++ genFlags ++ simplFlags) fp 
  env <- getSession
  mgSimpl <- liftIO $ core2core env (mgCore {mg_binds = (mg_binds mgCore)})
  return (mg_binds mgSimpl, ref)

toDesugar :: FilePath -> Ghc (CoreProgram, IORef [Warning])
-- | Desugar and return coreprogram and warnings  
toDesugar fp = 
    (toDesugar' False (holeFlags ++ genFlags) fp) >>= \(mg,ref) -> return (mg_binds mg,ref)


compSimplNormalised :: ExerciseName -> FilePath -> IO (CoreProgram, [Warning])
-- | Compile a file to normalised Core with simplifier
compSimplNormalised name fp = runGhc (Just libdir) $ do 
  (prog,ref) <- toSimplify fp 
  prog' <- liftIO $ normalise name prog 
  ws <- liftIO (readIORef ref)
  return (removeTyEvidence prog', ws)

compDesNormalised :: ExerciseName -> FilePath -> IO (CoreProgram, [Warning])
-- | Compile a file to normalised Core without simplifier
compDesNormalised name fp = runGhc (Just libdir) $ do 
  (prog,ref) <- toDesugar fp 
  prog' <- liftIO $ normalise name prog
  ws <- liftIO (readIORef ref)
  return (removeTyEvidence prog', ws)

compSimpl :: ExerciseName -> FilePath -> IO (CoreProgram, [Warning])
-- | Compile to simplified core directly, return renamed program and warnings
compSimpl n fp = runGhc (Just libdir) $ do
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  env <- getSession
  setFlags True (holeFlags ++ genFlags ++ simplFlags)
  mod <- compileToCoreSimplified fp
  prog <- liftIO $ alpha n =<< removeModInfo (cm_binds mod)
  w <- liftIO $ readIORef ref 
  return (prog,w)

compDes :: ExerciseName -> FilePath -> IO (CoreProgram, [Warning])
-- | Compile to desugared core directly, return renamed program and warnings
compDes n fp = runGhc (Just libdir) $ do
  ref <- liftIO (newIORef [])
  pushLogHookM (writeWarnings ref)
  env <- getSession
  setFlags False (holeFlags ++ genFlags)
  mod <- compileToCoreModule fp
  prog <- liftIO $ alpha n =<< removeModInfo (cm_binds mod)
  w <- liftIO $ readIORef ref 
  return (prog,w)


--- For testing purposes 

compString :: String -> ExerciseName -> IO (CoreProgram, [Warning])
compString input exercise = do 
  rules <- readFile "Rules.hs"
  let inputstr = "module Temp where\n" `nl` input `nl` rules
  handle <- openFile "studentfiles/Temp.hs" WriteMode
  hPutStrLn handle inputstr
  hFlush handle
  hClose handle
  compSimplNormalised exercise "./studentfiles/Temp.hs"


compTestNorm :: (FilePath -> Ghc (CoreProgram, IORef [Warning]))
         -> ExerciseName -> FilePath -> IO (CoreProgram, HscEnv)
-- | Compile with given function, return normalised program and environment
compTestNorm f n fp = runGhc (Just libdir) $ do
  (prog,_) <- f fp 
  prog' <- liftIO $ normalise n prog 
  env <- getSession 
  return (prog',env)


typeCheckCore :: CoreProgram -> HscEnv -> IO ()
-- | Use Core Linter to check for problems
typeCheckCore coreprog env = do
   let coretodo = CoreDoPasses [CoreDesugar, CoreDesugarOpt, CoreTidy, CorePrep]
       dflags = hsc_dflags env
   unless (gopt Opt_DoCoreLinting dflags) $ error "CoreLinting flag must be set"
   liftIO $ lintPassResult env coretodo (coreprog)
