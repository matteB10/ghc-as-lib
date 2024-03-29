{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-all #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use <&>" #-}

module Test where

import GHC.Core (CoreProgram)
import GHC (HscEnv, ParsedSource)
import GHC.Utils.Outputable

import Compile.Compile
import Transform.Transform
import Instances.Similar
import Instances.Diff ((~~))
import Utils.Utils
import Feedback.HoleMatches
import Feedback.PrintHoleMatch
import Compile.Warning
import Feedback.Feedback
import Feedback.Analyse 
import Utils.File  

import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute)
import System.FilePath ((</>), takeDirectory, takeExtension, takeBaseName)
import Data.List (isSuffixOf, isPrefixOf, sortOn)
import Data.Char (isDigit)
import Data.Ord
import Control.Lens (rewrite, over)
import Test.QuickCheck (ASCIIString(getASCIIString))
import Data.List ((\\))

import System.IO
import System.IO.Unsafe
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Control.Monad (when, unless)
import GHC.Driver.Session (programName, WarningFlag (..), WarnReason (..))
import GHC.Plugins
import Options.Applicative.Common (runParser)
import Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import Data.Map (Map)


path = "tests/testfiles"

testAll :: (ExerciseName -> FilePath -> IO CompInfo) -> IO ()
-- | Test and output number of matched tests
testAll compilefun = do
  exercises <- listDirectory path
  print exercises
  let f n expectation = uncurry $ compare_pr (compilefun n) expectation
  st <- mapM succTests exercises
  ft <- mapM failTests exercises
  nt <- mapM normaliseTests exercises
  sr <- mapM (\(p1,p2) -> f (getN p1) True (p1,p2)) (concat st)
  fr <- mapM (\(p1,p2) -> f (getN p1) False (p1,p2)) (concat ft)
  nr <- mapM (\(p1,p2) -> f (getN p1) True (p1,p2)) (concat nt)
  let totsucc = length (concat st)
      totfail = length (concat ft)
      totnorm = length (concat nt)
      success = length (filter id sr)
      fail    = length (filter id fr)
      norm    = length (filter id nr)
  putStrLn $ showRes success totsucc "success"
  putStrLn $ showRes fail totfail "fail"
  putStrLn $ showRes norm totnorm "success with normalisation"
  putStrLn $ "total: " ++ showRes (success + norm + fail) (totsucc + totnorm + totfail) "results"
    where showRes res tot expres = show res ++ "/" ++ show tot `sp` "of expected" `sp` expres
          getN = takeBaseName . takeDirectory . takeDirectory

toTuple :: CompInfo -> IO (CoreProgram, ParsedSource, [Warning], Map Var Var, String)
toTuple (CompInfo prog ps ws n e) = return (prog,ps,ws,n,e)

compare_pr :: (FilePath -> IO CompInfo) -> Bool -> FilePath -> FilePath -> IO Bool
compare_pr compile b fp1 fp2 = do
  minf <- compile fp1
  sinf <- compile fp2
  let pred = core sinf ~> core minf
      match = core sinf ~= core minf
      res = pred || match
  let feedback = mkFeedback sinf [minf]
  when (not res && b) $ putStrLn $ "failed to match : " ++ show fp1 `nl` show feedback
  return ((isMatch feedback || isOnTrack feedback) == b) -- feedback matches expected result


getExFiles :: ExerciseName -> IO [FilePath]
getExFiles n = do
  getFilePaths $ path ++ n

succTests :: ExerciseName -> IO [(FilePath, FilePath)]
succTests ename = matchSuffixedFiles (path++ename++"/good/")

failTests :: ExerciseName -> IO [(FilePath, FilePath)]
failTests ename = matchSuffixedFiles (path++ename++"/bad/")

normaliseTests :: ExerciseName -> IO [(FilePath, FilePath)]
normaliseTests ename = matchSuffixedFiles (path++ename++"/norm/")



-- ============= TEST REAL STUDENT SOLUTIONS =================


data TestItem = TestItem { exerciseid :: String
                         , input      :: String
                         , category   :: String
                         , typesig    :: String}
      deriving (Generic, Show)

instance ToJSON TestItem where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TestItem


decodeJson :: FilePath -> IO [TestItem]
decodeJson fp = do
  handle <- openBinaryFile fp ReadMode 
  inputJson <- B.hGetContents handle 
  let inputData = decode inputJson :: Maybe [TestItem]
  case inputData of
    Just d -> return d 
    Nothing -> error $ "Could not read " ++ fp

msPath = "./modelsolutions/"

testItems :: (ExerciseName -> FilePath -> IO CompInfo) -> FilePath -> IO ()
testItems cfun jsonfile = do
  items <- decodeJson jsonfile
  results <- mapM (testItem cfun) items
  let exps = filter (\(x,y,z) -> (x,y,z)==(True,True,z)) results -- same results as in Ask-Elle 
      expf = filter (\(x,y,z) -> (x,y,z)==(False,False,z)) results
      succ = filter (\(x,y,z) -> (x,y,z)==(False,True,z)) results
      fail = filter (\(x,y,z) -> (x,y,z)==(True,False,z)) results
      nboftest = f items
      f = show . length
  putStrLn "printing failed attempts:"
  mapM_ (\(_,_,prog) -> printProg prog) fail
  putStrLn "printing expected failed attempts:"
  mapM_ (\(_,_,prog) -> printProg prog) expf
  putStrLn $ f exps ++ "/" ++ nboftest ++ " tests gave same successful result as Ask-Elle"
  putStrLn $ f expf ++ "/" ++ nboftest ++ " tests gave same failed result as Ask-Elle"
  putStrLn $ f succ ++ "/" ++ nboftest ++ " could now be matched"
  putStrLn $ f fail ++ "/" ++ nboftest ++ " expected to match, but failed"

testItem :: (ExerciseName -> FilePath -> IO CompInfo) -> TestItem -> IO (Bool,Bool,String)
-- | (True,True) : Success in Ask-Elle, Success in ghc 
--   (False,False) : Unknown in Ask-Elle, Failure in ghc 
--   (False, True) : Unknown in Ask-Elle, success in ghc 
testItem f ti = do
    path <- makeAbsolute tmpPath
    writeInput path (input ti)
    let exercisename = takeBaseName (exerciseid ti)
    hasTypSig <- checkTypeSig path exercisename
    unless hasTypSig (writeProg path ti)
    (stProg,psrc,ws,_,_) <- toTuple =<< f exercisename path  -- student progrm
    modelFiles <- getFilePaths (msPath ++ exerciseid ti)
    mProgs <- mapM (f exercisename) modelFiles
    let pred = any ((stProg ~>) . core) mProgs  -- is predecessor to any of the model solutions
        match = any ((stProg ~=) . core) mProgs -- is similar to any of the model solutions
        res =  pred || match
    --print res 
    --printRedundantPat ti stProg (zip mProgs modelFiles)
    case category ti of
        "Complete"   -> return (True, match,input ti)  -- program completed
        "OnTrack"    -> return (True, res,input ti)  -- recognised as on track
        "Missing"    -> return (False,res,input ti)  -- missing cases (not defined on all input)
        "TestPassed" -> return (False,res,input ti)  -- quickcheck tests passed, but could not be matched 
        "Unknown"    -> return (False,res,input ti)  -- unknown



-- | take an exercise name and a path to the file, 
--   compare it with its model solutions
printRedundantPat :: TestItem -> CoreProgram -> [(CoreProgram, FilePath)] -> IO ()
printRedundantPat ti stProg modProgfiles = do
    let (closest,cfile) = getSmallestP $ map (g stProg) modProgfiles -- get model prog with smallest diff 
    let hasRedPat = closest `hasRedundantPattern` stProg
    when hasRedPat (do
      putStrLn "redundant pattern in student prog:"
      putStrLn (input ti)
      putStrLn "tried to match with model:"
      putStrLn cfile
      putStrLn "-------------------------")





g p1 (p2,file) = ((p2,file), p1 ~~ p2) -- check diff 


{- minBy :: Ord a => (b -> a) -> b -> b -> b
minBy f x y
  | f x <= f y = x
  | otherwise = y -}

getSmallestP :: [((CoreProgram,FilePath), Int)] -> (CoreProgram,FilePath)
getSmallestP programs = fst $ foldr1 (minBy snd) programs

printProg :: Show a => a -> IO ()
printProg prog = do
     putStrLn "----------------------------------"
     print prog
     putStrLn "----------------------------------\n"

tempHeader = "module Temp where\n"

writeProg :: FilePath -> TestItem -> IO ()
writeProg fp ti = do
    rules <- readFile =<< makeAbsolute rulesFile 
    let inputstr = tempHeader ++ typesig ti `nl` input ti `nl` rules
    -- Write the inputstr to the temporary file
    handle <- openFile fp WriteMode
    hPutStrLn handle inputstr
    hFlush handle
    hClose handle


diagnose :: FilePath -> IO () 
diagnose fp = decodeJson fp >>= analyseItems


analyseItems :: [TestItem] -> IO ()
analyseItems items = do
  results <- mapM analyse' items
  let match = length $ filter isMatch results
      ontrack = length $ filter isOnTrack results
      missing = length $ filter isMissingCases results
      unknown = length $ filter isUnknown results
      overlapping = length $ filter isOverLapping results
      holesugg = length $ filter isHoleMatch results 
      hlint = length $ filter isHLint results
      tot = length items
  putStrLn $ show match ++ "/" ++ show tot ++ " matched."
  putStrLn $ show ontrack ++ "/" ++ show tot ++ " is on track."
  putStrLn $ show missing ++ "/" ++ show tot ++ " has missing cases."
  putStrLn $ show overlapping ++ "/" ++ show tot ++ " has overlapping patterns."
  putStrLn $ show unknown ++ "/" ++ show tot ++ " is unknown."
  putStrLn $ show holesugg ++ "/" ++ show tot ++ "got hole suggestions"
  putStrLn $ show hlint ++ "/" ++ show tot ++ "got Hlint suggestions"
  

tmpPath :: FilePath
-- relative paths
tmpPath    = "tests/studentfiles/Temp.hs"
modelsPath = "modelsolutions/"


analyse' :: TestItem -> IO Feedback
analyse' ti = if null (input ti) then return (Ontrack []) else (do
     path <- makeAbsolute tmpPath
     writeInput path (input ti)
     let exercisename = takeBaseName (exerciseid ti)
     hasTypSig <- checkTypeSig path exercisename
     modelFiles <- getFilePaths (modelsPath ++ exerciseid ti)
     mInfs <- mapM (compile exercisename) modelFiles
     when (null modelFiles) $ error ("could not read modelfiles from" ++ exerciseid ti)
     typsig <- parseExerciseTypSig (head modelFiles) exercisename
     unless hasTypSig (writeProg path (ti {typesig = typsig}))
     stInf <- compile exercisename path  -- student progrm
     let feedback = mkFeedback stInf mInfs
     putStrLn "------------------------------- "
     putStrLn (input ti) >> print feedback
     putStrLn "------------------------------- "
     return feedback)


analyse :: String -> ExerciseName -> CompileFun -> IO (CompInfo, CompInfo, Feedback)
analyse input exercise f = do
  let exercisename = takeBaseName exercise
  stInf <- compString input exercisename  -- student progrm
  modelFiles <- getFilePaths (msPath ++ exercise)
  modInfo <- mapM (f exercisename) modelFiles
  let mInf = getClosest stInf modInfo
  liftIO $ print $ core mInf 
  let feedback = mkFeedback stInf modInfo
  return (stInf, mInf, feedback)

analyse_ :: FilePath -> ExerciseName -> IO (CompInfo, CompInfo, Feedback)
analyse_ fp exercise = do
  let exercisename = takeBaseName exercise
  stInf <- compile exercisename fp 
  modelFiles <- getFilePaths (msPath ++ exercise)
  modInfo <- mapM (compile exercisename) modelFiles
  let mInf = getClosest stInf modInfo
  let feedback = mkFeedback stInf modInfo
  return (stInf, mInf, feedback)


getBinder :: CoreProgram -> String -> [CoreBind]
getBinder p s = let recs = filter isRect p 
                    nonr = filter isNonRect p 
                    rs = filter (\x -> getOccString (getBindTopVar x) == s) recs 
                    ns = filter (\x -> getOccString (getBindTopVar x) == s) nonr 
                in rs ++ ns 


isNonRect b = case b of {(Rec es) -> True; _ -> False}

isRect = not . isNonRect  


--- For testing purposes 
type CompileFun = ExerciseName -> FilePath -> IO CompInfo

compString :: String -> ExerciseName -> IO CompInfo
-- compile a program given as a string
compString input exercise = do 
  rules <- readFile "Rules.hs"
  let inputstr = "module Temp where\n" `nl` input `nl` rules
  handle <- openFile "studentfiles/Temp.hs" WriteMode
  hPutStrLn handle inputstr
  hFlush handle
  hClose handle
  compile exercise "./studentfiles/Temp.hs"