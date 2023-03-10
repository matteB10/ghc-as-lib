{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- disable warnings from unrecognised pragma in model solution files 
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

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
      GhcMonad(getSession),
      DynFlags(..),
      SuccessFlag,
      DesugaredModule (dm_core_module, dm_typechecked_module),
      compileToCoreSimplified,
      parseExpr, 
      CoreModule (cm_binds, cm_types), pprVarSig, compileToCoreModule, SrcLoc, getContext, getRealSrcSpan, lookupName, TypecheckedSource)

--import GHC.Show
import GHC.Paths (libdir)
import GHC.Data.Bag ()

import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut, gopt_set, optimisationFlags)
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
import Transform ( alpha, removeModInfo, eta )
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
holeFlags =
  [ Opt_ShowHoleConstraints,
    Opt_ShowProvOfHoleFits,
    Opt_ShowTypeAppVarsOfHoleFits,
    Opt_ShowTypeAppOfHoleFits,
    Opt_ShowTypeOfHoleFits
  ]

genFlags :: [GeneralFlag]
genFlags = [Opt_DeferTypedHoles, Opt_DoEtaReduction, Opt_DoLambdaEtaExpansion]

compExpr :: String -> IO ()
compExpr expr = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  e <- parseExpr expr
  undefined 

setFlags :: Bool -> Ghc ()
setFlags b = do 
   dflags <- getSessionDynFlags 
   let flags = if b then EnumSet.toList (generalFlags dflags) ++ addFlags  else addFlags  
       addFlags = holeFlags ++ genFlags
       dflags'  = dflags {refLevelHoleFits = Just 2,
                          maxValidHoleFits = Just 8,
                          maxRefHoleFits   = Just 15,
                          generalFlags = EnumSet.fromList flags} 

   return ()   

comp :: FilePath -> IO ()
-- | Compile to Core, print additional information
comp file = runGhc (Just libdir) $ do
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

  liftIO $ putStrLn $ "HolefitPlugins: " ++ show (tcg_hf_plugins tcenv)

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
  --liftIO $ findLiterals coreprog

  liftIO $ banner "Print hole loc"
  liftIO $ findHoles coreprog


compTest :: FilePath -> IO String
compTest file = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gen_flags = Opt_DeferTypedHoles `EnumSet.insert` flags --Opt_DoEtaReduction seems to have no effect ?
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = gen_flags,
                        dumpFlags = EnumSet.fromList [Opt_D_dump_simpl_stats, Opt_D_dump_simpl_iterations],
                        simplPhases = 1}
  --liftIO $ print dflags' 
  setSessionDynFlags dflags'
  target <- guessTarget file Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName file)

  pmod <- parseModule modSum
  tmod <- typecheckModule pmod
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = dm_core_module dmod  -- :: ModGuts 
  let coreprogram = mg_binds core
  let str = (findLiterals coreprogram)
  let file_out = replaceDirectory file "./out/"
  liftIO $ writeFile file_out $ show coreprogram -- write AST to file 
  return str


banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"



compToFile :: FilePath -> IO ()
compToFile fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let opt_flags = EnumSet.fromList $ [Opt_DeferTypedHoles,
                                      Opt_DoEtaReduction,
                                      Opt_ShowValidHoleFits,
                                      Opt_ShowTypeOfHoleFits]
                                      ++ EnumSet.toList (generalFlags dflags)
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = opt_flags} -- set dynflags directly 
  setSessionDynFlags dflags'
  {- Gives expection from type error, despite defer typed holes flag 
  core <- compileToCoreModule "./src/Test2.hs"
  liftIO $ banner "Deferred type error"
  liftIO $ findLiterals (cm_binds core)
  -}
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum
  tmod <- typecheckModule pmod
  dmod <- desugarModule tmod
  let core = dm_core_module dmod :: ModGuts
  let coreprog = mg_binds core   :: CoreProgram
  let file_out = replaceDirectory fp "./out/"
  liftIO $ writeFile file_out $ show coreprog



compToTc :: FilePath -> IO ()
compToTc fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let flags = EnumSet.delete Opt_KeepOFiles $ EnumSet.delete Opt_KeepHiFiles (generalFlags dflags)
  let gen_flags = Opt_DeferTypedHoles `EnumSet.insert` flags --Opt_DoEtaReduction seems to have no effect ?
  let dflags' = dflags {refLevelHoleFits = Just 2,
                        maxValidHoleFits = Just 8,
                        maxRefHoleFits   = Just 15,
                        generalFlags = gen_flags}
  setSessionDynFlags dflags'
  target <- guessTarget fp Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName (takeBaseName fp)
  pmod <- parseModule modSum
  tmod <- typecheckModule pmod
  -- tm_renamed_source :: Maybe RenamedSource
  let tprogram = tm_typechecked_source tmod 
  liftIO $ putStrLn $ showGhc dflags' tprogram
  --return tprogram 

compSimpl :: Bool -> FilePath -> IO CoreProgram
-- | Compile to Core, after simplifier pass 
compSimpl defaultflags file = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  let dump_flags = EnumSet.fromList [Opt_D_dump_simpl_stats]
  let gen_flags = (if defaultflags
                      then generalFlags dflags
                      else EnumSet.fromList [Opt_DeferTypedHoles,
                      Opt_DoEtaReduction,
                      Opt_DoLambdaEtaExpansion,
                      Opt_ShowHoleConstraints,
                      Opt_ShowValidHoleFits,
                      Opt_SortValidHoleFits,
                      Opt_SortBySizeHoleFits,
                      Opt_ShowTypeAppOfHoleFits,
                      Opt_ShowTypeOfHoleFits,
                      Opt_ShowProvOfHoleFits,
                      Opt_ShowMatchesOfHoleFits])
  let dflags' = dflags {refLevelHoleFits = Just 2
                        ,maxValidHoleFits = Just 1
                        ,maxRefHoleFits   = Just 1
                        ,generalFlags = gen_flags
                        --,dumpFlags = dump_flags
                        }
  setSessionDynFlags dflags'
  simpl <- compileToCoreSimplified file
  --liftIO $ banner "Simplified ppr"
  --liftIO $ putStr $ showGhc dflags' (cm_binds simpl)
  --liftIO $ banner "Core type env" -- only top level type bindings
  --liftIO $ putStrLn $ showGhc dflags (cm_types simpl)
  --liftIO $ banner "Simplified AST"
  return (cm_binds simpl)


compToSimpl :: FilePath -> IO CoreProgram
compToSimpl = compSimpl False

compDefSimpl :: FilePath -> IO CoreProgram
compDefSimpl = compSimpl True

compToCore :: FilePath -> IO CoreProgram
compToCore fp = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  --let dump_flags = EnumSet.fromList [Opt_D_dump_simpl_stats]

  let gen_flags = EnumSet.fromList [Opt_DeferTypedHoles,
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
  let dflags' = dflags {refLevelHoleFits = Just 2
                        ,maxValidHoleFits = Just 1
                        ,maxRefHoleFits   = Just 1
                        ,generalFlags = gen_flags
                        --,dumpFlags = dump_flags
                        }
  setSessionDynFlags dflags'
  coremod <- compileToCoreModule fp
  --liftIO $ banner "Core ppr"
  --liftIO $ putStrLn $ showGhc dflags (cm_binds coremod)
  --liftIO $ banner "Core type env" -- only top level type bindings
  --liftIO $ putStrLn $ showGhc dflags (cm_types coremod)
  return (cm_binds coremod)


findLiterals :: CoreProgram -> String
findLiterals cs = concat (concatMap findLits cs)

findLits :: Bind CoreBndr -> [String]
findLits (NonRec b exps)  = findLit exps
findLits (Rec es)         = concatMap (findLit . snd) es

findLit :: Expr CoreBndr -> [String]
findLit (Var id)     = []
findLit (Lit l)      = [show l] -- ++ "\n"-- here is type errors stored, print them
findLit (App e bndr) = findLit e ++ findLit bndr
findLit (Lam b e)    = findLit e
findLit (Let bi e)   = findLits bi ++ findLit e
findLit (Case e _ _ alts) = findLit e ++ concatMap (\(Alt _ _ e) -> findLit e) alts --Alt AltCon [b] (Expr b)
findLit (Cast  e _)  = findLit e
findLit (Tick _ e)   = findLit e
findLit (Type t)     = []
findLit (Coercion c) = []

findHoles :: CoreProgram -> IO ()
findHoles = mapM_ findHoles'

findHoles' :: Bind CoreBndr -> IO ()
findHoles' (NonRec _ exps)  = findHole exps
findHoles' (Rec es)         = mapM_ (findHole . snd) es

findHole :: Expr CoreBndr -> IO ()
findHole (Var var) | getOccString (varName var) == "typeError" = return (){-
                          putStrLn ("typedHole loc: " ++ show (getSrcLoc var)
                              ++ (" , occname: " ++ getOccString name
                              ++ " , unique: " ++ show (nameUnique name)
                              ++ " , type: ")) 
                              >> liftIO ( runGhc (Just libdir) $ do
                                      env <- getSession
                                      dflags <- getSessionDynFlags --
                                      let opt_flags = EnumSet.fromList [Opt_DeferTypedHoles, Opt_DoEtaReduction]
                                      let dflags' = dflags {refLevelHoleFits = Just 2, generalFlags = opt_flags, simplPhases = 1} -- set dynflags directly 
                                      setSessionDynFlags dflags'
                                      mn <- lookupName name
                                      liftIO $ putStr "Tything: "
                                      liftIO $ print mn) -}
                   | otherwise =  return () --putStrLn ("occname: " ++ getOccString name ++ " , unique: " ++ show (nameUnique name))
                        where name = varName var
findHole (Lit l)            = when (isTypedHolErrMsg l) $ let ((r,c), t) = holeTypeFromMsg (show l)
                                                          in putStrLn $ "found hole at " ++ p (r ++ ":" ++ c) ++ " with type: " ++ t
findHole (App e bndr)       = findHole e >> findHole bndr
findHole (Lam b e)          = findHole e
findHole (Let bi e)         = findHoles' bi >> findHole e
findHole (Case e _ _ alts)  = findHole e >> mapM_ (\(Alt _ _ e) -> findHole e) alts --alts :: Alt AltCon [b] (Expr b)
findHole (Cast  e _)        = findHole e
findHole (Tick _ e)         = findHole e
findHole (Type t)           = return ()
findHole (Coercion c)       = return ()


isTypedHolErrMsg :: Literal -> Bool
isTypedHolErrMsg (LitString l) = let msg = lines $ utf8DecodeByteString l
                                 in last msg == "(deferred type error)"
isTypedHolErrMsg _ = False


holeTypeFromMsg :: String -> ((String, String), String)
holeTypeFromMsg s = ((row,col), htype) --trace (show (lines s) ++ show (row,col,htype)) 
    where (l1:l2:ls) = if length (lines s) > 1 then lines s else ["no hole", "is found"]
          rm    =  drop 1 (dropWhile (/= ':') l1)
          row   = takeWhile (/= ':') rm
          col   = takeWhile (/= ':') (drop (length row + 1) rm)
          hname = takeWhile (/= ' ') (drop 2 (dropWhile (/= ':') l2)) -- if hole suffixed with identifier
          htype = drop 3 $ dropWhile (/= ':') (drop 1 (dropWhile (/= ':') l2))


compPrint compile fp = do 
  p <- readFile fp
  p' <- compile fp
  let p'' = removeModInfo p'
  let erp = applymany eta p'' 
  banner "student prog:"
  putStrLn p
  putStrLn "Holes:"
  findHoles p'' 
  banner "Core AST"
  print p''
  banner "Eta-reduced"
  print erp
  return erp 


compare_print :: (FilePath -> IO CoreProgram) -> FilePath -> FilePath -> IO ()
compare_print compile fp1 fp2 = do
  p1 <- readFile fp1
  p2 <- readFile fp2
  cp1' <- compile fp1
  let cp1 = removeModInfo cp1'
  cp2' <- compile fp2
  let cp2 = removeModInfo cp2'
  banner "student prog:"
  putStrLn p1
  findHoles cp1
  banner "model prog: "
  putStrLn p2
  findHoles cp2
  banner "check match"
  putStrLn "student prog:"
  print cp1
  putStrLn "model prog:"
  print cp2 
  putStrLn $ "Programs match: " ++ show (cp1 ~== cp2)

compare_ :: (FilePath -> IO CoreProgram) -> (CoreProgram -> CoreProgram) -> FilePath -> FilePath -> IO ()
compare_ comp_pass transf fp1 fp2 = do
  p1 <- readFile fp1
  p2 <- readFile fp2
  cp1' <- comp_pass fp1
  let cp1 = applymany transf (removeModInfo cp1')
  cp2' <- comp_pass fp2
  let cp2 = applymany transf (removeModInfo cp2')
  banner "student prog:"
  putStrLn p1
  banner "model prog: "
  putStrLn p2
  banner "check match"
  putStrLn "student prog:"
  print cp1
  putStrLn "model prog:"
  print cp2 
  putStrLn $ "Programs match: " ++ show (cp1 ~== cp2)


compileAndSimplify :: (FilePath -> IO CoreProgram) -> FilePath -> IO CoreProgram
compileAndSimplify compile fp = do
  p <- readFile fp
  cp <- compile fp 
  let cp' = alpha $ applymany eta (removeModInfo cp)
  --banner "src prog:"
  --putStrLn p
  --banner "AST core: "
  --print cp 
  --banner "AST remove mod: "
  let rcp = removeModInfo cp 
  --print rcp
  --banner "AST eta-red"
  let ecp = applymany eta rcp 
  --print ecp 
  --banner "AST alpha"
  let acp = alpha ecp "dupli" 
  --print acp 
  return acp


applymany :: (CoreProgram -> CoreProgram) -> CoreProgram -> CoreProgram
applymany eta p | show (eta p) == show p = p  -- replace with sim relation later        
                | otherwise     = applymany eta (eta p)

compare_desugar, compare_simpl :: FilePath -> FilePath -> IO ()
compare_desugar = compare_ compToCore id 
compare_simpl = compare_ compToSimpl id 
compare_eta :: FilePath -> FilePath -> IO ()
compare_eta = compare_ compToCore (eta)

compileSim :: FilePath -> IO CoreProgram
compileSim = compileAndSimplify compToSimpl 

compareSim :: FilePath -> FilePath -> IO ()
compareSim fp1 fp2 = do 
    p1 <- compileSim fp1 
    print p1 
    putStrLn ""
    p2 <- compileSim fp2
    print p2 
    putStrLn $ "Match: " ++ show (p1 ~== p2)

main :: IO ()
main = do
  --testSuccess
  --testFailure  
  --putStrLn "Eta-test: EXPECTED: fail, fail, success, success"
  mapM_ (uncurry compare_simpl) etatest 
  mapM_ (uncurry compare_desugar) etatest
  mapM_ (uncurry (compare_eta)) etatest

testSuccess :: IO () 
testSuccess = do 
  putStrLn "Test success desugar:"
  mapM_ (uncurry compare_desugar) succtest
  putStrLn "Test success simpl:"
  mapM_ (uncurry compare_simpl) succtest
  putStrLn "Test success desugar eta-red:"
  mapM_ (uncurry (compare_eta)) succtest 

testFailure :: IO ()
testFailure = do 
  putStrLn "Test Failure desugar:"
  mapM_ (uncurry compare_desugar) failtest
  putStrLn  "Test Failure simpl:" 
  mapM_ (uncurry compare_simpl) failtest
  putStrLn  "Test Failure desugar eta:"
  mapM_ (uncurry (compare_eta)) failtest


s1, s2, s3, s4, s5, s6 :: Pair
f1, f2 :: Pair
t1 :: Pair

type Pair = (String,String)

pair :: String -> String -> Pair
pair x y = (x,y)

-- should succeed 
s1 = pair "dupli/Test1.hs" "dupli/Mod1.hs"
s2 = pair "dupli/Test2.hs" "dupli/Mod2.hs"
s3 = pair "dupli/Test4.hs" "dupli/Mod4.hs" -- inlining 
s4 = pair "dupli/Test6.hs" "dupli/Mod5.hs" -- hole match with anything
s5 = pair "dupli/Test5.hs" "dupli/Mod6.hs" -- hole match with anything 
s6 = pair "myreverse/Test1.hs" "myreverse/Mod1.hs"
succtest = [s1,s2,s3,s4,s5]

-- should fail 
f1 = pair "dupli/Test3.hs" "dupli/Mod3.hs" -- one recursive one not 
f2 = pair "dupli/Test5.hs" "dupli/Mod5.hs" -- one use foldr other one foldl
failtest = [f1,f2]

-- should succeed, does with manual eta reduction (but not GHC's) 
t1 = pair "dupli/Test7.hs" "dupli/Mod3.hs"
t2 = pair "dupli/Test8.hs" "dupli/Mod8.hs"
etatest = [t1,t2]

