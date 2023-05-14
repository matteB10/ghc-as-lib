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

import Compile
import Data.Ord
import Transform
import Similar
import Utils
import HoleMatches
import Print

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, takeExtension, takeBaseName)
import Data.List (isSuffixOf, isPrefixOf, sortOn)
import Data.Char (isDigit)
import Control.Lens (rewrite)
import Test.QuickCheck (ASCIIString(getASCIIString))
import Data.List ((\\))

import System.IO
import System.IO.Temp
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Control.Monad (when, unless)
import Diff ((~~))
import Analyse (hasRedundantPattern)
import GHC.Driver.Session (programName, WarningFlag (..), WarnReason (..))
import GHC.Plugins 
import Warning ( Warning(reason) ) 
import Feedback
    ( Feedback, mkFeedback, isMatch, isOnTrack, getClosest ) 
import Options.Applicative.Common (runParser)
import Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import Data.Map (Map)

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



testTc :: FilePath -> (ExerciseName -> FilePath -> IO (CoreProgram, HscEnv)) -> IO ()
testTc fp f = do
    files <- getFilePaths fp
    compiled <- mapM (f "") files
    mapM_ tcCore compiled
    putStrLn "All tests typechecked"

testTcAll :: (ExerciseName -> FilePath -> IO (CoreProgram, HscEnv)) -> IO ()
-- | Core lint (typecheck) core program after applying given function f 
testTcAll f = do
    files <- getFilePaths path
    compiled <- mapM (\x -> f (getN x) x) files
    mapM_ tcCore compiled
    putStrLn $ "All" `sp` show (length compiled) `sp` "tests typechecked"
    where getN = takeBaseName . takeDirectory . takeDirectory

tcCore :: (CoreProgram, HscEnv) -> IO ()
tcCore = uncurry typeCheckCore

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


matchSuffixedFiles :: FilePath -> IO [(FilePath, FilePath)]
matchSuffixedFiles folderPath = do
  files <- listDirectory folderPath
  let testFiles = filter (\f -> "Test" `isPrefixOf` f) files
  let modFiles = filter (\f -> "Mod" `isPrefixOf` f) files
  let matchingTuples = [(dir </> f1, dir </> f2) | f1 <- modFiles, f2 <- testFiles, extractSuffix f1 == extractSuffix f2]
  return matchingTuples
  where
    dir = takeDirectory folderPath


extractSuffix :: FilePath -> String
extractSuffix filePath =
  let reversedFile = reverse filePath
      reversedSuffix = takeWhile (/='.') reversedFile ++ "."
      revSuffWNum = takeWhile isDigit (drop (length reversedSuffix) reversedFile)
  in reverse (reversedSuffix ++ revSuffWNum)


getFilePaths :: FilePath -> IO [FilePath]
getFilePaths folderPath = do
  isDir <- doesDirectoryExist folderPath
  if isDir
    then do
      dircontent <- listDirectory folderPath
      subFiles <- mapM getFilePaths [folderPath </> f | f <- dircontent]
      let content = map (folderPath </>) dircontent ++ concat subFiles
      return $ filter (\fp -> (".hs" `isSuffixOf` fp) && not (isConfig fp) && isFile fp) content
    else
      return []
    where isConfig fp = "Config.hs" `isSuffixOf` fp

isFile :: FilePath -> Bool
isFile path = not (null (takeExtension path))

path = "./testfiles/"

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
  inputJson <- B.readFile fp
  let inputData = decode inputJson :: Maybe [TestItem]
  case inputData of
    Just d -> return (take 100 d) -- more than 100 will eat all the memory
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
    writeProg ti
    let exercisename = takeBaseName (exerciseid ti)
    (stProg,psrc,ws,_,_) <- toTuple =<< f exercisename "./studentfiles/Temp.hs"  -- student progrm
    modelFiles <- getFilePaths (msPath ++ exerciseid ti)
    mProgs <- mapM (f exercisename) modelFiles
    let pred = any ((stProg ~>) . core) mProgs  -- is predecessor to any of the model solutions
        match = any ((stProg ~=) . core) mProgs -- is similar to any of the model solutions
        res =  pred || match
    --print res 
    --printRedundantPat ti stProg (zip mProgs modelFiles)
    case category ti of
        "Complete"   -> return (True, res,input ti)  -- program completed
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


minBy :: Ord a => (b -> a) -> b -> b -> b
minBy f x y
  | f x <= f y = x
  | otherwise = y

getSmallestP :: [((CoreProgram,FilePath), Int)] -> (CoreProgram,FilePath)
getSmallestP programs = fst $ foldr1 (minBy snd) programs

printProg :: Show a => a -> IO ()
printProg prog = do
     putStrLn "----------------------------------"
     print prog
     putStrLn "----------------------------------\n"

tempHeader = "module Temp where\n"

writeProg :: TestItem -> IO ()
writeProg ti = do
    rules <- readFile "Rules.hs"

    let inputstr = tempHeader ++ typesig ti `nl` input ti `nl` rules
    -- Write the inputstr to the temporary file
    handle <- openFile "studentfiles/Temp.hs" WriteMode
    hPutStrLn handle inputstr
    hFlush handle
    hClose handle

writeInput :: String -> IO ()
writeInput input = do
    rules <- readFile "Rules.hs"
    -- must insert type signature in a better way later, if not defined by student 
    let inputstr = tempHeader `nl` input `nl` rules
    -- Write the inputstr to the temporary file
    handle <- openFile "studentfiles/Temp.hs" WriteMode
    hPutStrLn handle inputstr
    hFlush handle
    hClose handle

analyseItems :: FilePath -> IO ()
analyseItems jsonfile = do
  items <- decodeJson jsonfile
  results <- mapM analyse' items
  let match = length $ filter isMatch results
      ontrack = length $ filter isOnTrack results
      tot = length items
  putStrLn $ show match ++ "/" ++ show tot ++ " matched."
  putStrLn $ show ontrack ++ "/" ++ show tot ++ " is on track."

analyse' :: TestItem -> IO Feedback
analyse' ti = do
  writeInput (input ti)
  let exercisename = takeBaseName (exerciseid ti)
  hasTypSig <- checkTypeSig "./studentfiles/Temp.hs" exercisename
  modelFiles <- getFilePaths (msPath ++ (exerciseid ti))
  mInf <- mapM (compSimplNormalised exercisename) modelFiles
  typsig <- parseExerciseTypSig (head modelFiles) exercisename
  unless hasTypSig (writeProg (ti {typesig = typsig}))
  stInf <- compSimplNormalised exercisename "./studentfiles/Temp.hs"  -- student progrm
  
  --let pred = any ((stProg ~>) . core) mProgs  -- is predecessor to any of the model solutions
  --    match = any ((stProg ~=) . core) mProgs -- is similar to any of the model solutions
  let feedback = mkFeedback stInf mInf
  putStrLn "------------------------------- "
  putStrLn (input ti) >> print feedback
  putStrLn "------------------------------- "
  return feedback


analyse :: String -> ExerciseName -> IO Feedback
analyse input exercise = do
  writeInput input
  let exercisename = takeBaseName exercise
  stInfo <- compSimplNormalised exercisename "./studentfiles/Temp.hs"  -- student progrm
  modelFiles <- getFilePaths (msPath ++ exercise)
  modInfo <- mapM (compSimplNormalised exercisename) modelFiles
  let clos = getClosest stInfo modInfo
  putStrLn $ "Closest match\n" ++ show (core clos )
  {- let pred = any ((stProg ~>) . core) mProgs  -- is predecessor to any of the model solutions
      match = any ((stProg ~=) . core) mProgs -- is similar to any of the model solutions
      modProgFiles = zip (map core mProgs) modelFiles
      (closest,cfile) = getSmallestP $ map (g stProg) modProgFiles -- get model prog with smallest diff 
      hasRedPat = closest `hasRedundantPattern` stProg -}
  --print $ cfile 
  return $ mkFeedback stInfo modInfo