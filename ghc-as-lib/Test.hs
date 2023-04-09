{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <&>" #-}
module Test where


import GHC.Core (CoreProgram)
import GHC (HscEnv)
import GHC.Utils.Outputable

import Compile
import Transform
import Similar
import Utils
import HoleMatches
import PrettyPrint

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Char (isDigit)
import Control.Lens (rewrite)
import Test.QuickCheck (ASCIIString(getASCIIString))



data Mode = DEBUG -- print which pair of files compared, and the result 
          | RES   -- print only the result of all good/bad/norm tests 


test :: Mode -> ExerciseName -> IO ()
-- | Main test-function,
-- takes the name of the exercise as a string 
-- requires that folder with exercise name exist, with subfolders good, bad and norm. 
test m n = do
  let tf = case m of
          DEBUG -> testPr
          RES   -> testPrA
  succTests n >>= \f -> banner "Expected succesful match: "
              >> tf n f
  failTests n >>= \f -> banner "Expected failure: "
              >> tf n f
  normaliseTests n >>= \f -> banner "Expected match with normalisation: "
              >> tf n f


testTc :: ExerciseName -> IO ()
testTc fn = do
    files <- getExFiles fn
    compiled <- mapM compNorm files
    mapM_ tcCore compiled
    putStrLn "All tests typechecked"

testTcAll :: IO ()
testTcAll = do
    files <- getFilePaths path
    compiled <- mapM compNorm files
    mapM_ tcCore compiled
    putStrLn $ "All" `sp` show (length compiled) `sp` "tests typechecked"

tcCore :: (CoreProgram, HscEnv) -> IO ()
tcCore pe = uncurry typeCheckCore pe -- >> putStrLn "Typecheck Ok"

testPr :: ExerciseName -> [(FilePath,FilePath)] -> IO ()
-- | Test and print test by test 
testPr _ [] = return ()
testPr n (t:ts) = testPr' n t >> testPr n ts
  where testPr' n ps@(p1,p2) = do
          putStrLn $ "Comparing programs " ++ show p1 `sp` show p2
          putStrLn "Desugar:"
          putStr "programs match: " >> compare_desugar ps >>= print
          putStrLn  "Simplifier:"
          putStr "programs match: " >> compare_simpl ps >>= print
          putStrLn  "Manual transformations:"
          putStr "programs match: " >> compare_norm ps >>= print

testPrA :: ExerciseName -> [(FilePath,FilePath)] -> IO ()
-- | Test and print all  
testPrA n ps = do
  putStrLn "Desugar:"
  putStr "programs match" >> mapM compare_desugar ps >>= print
  putStrLn  "Simplifier:"
  putStr "programs match" >> mapM compare_simpl ps >>= print
  putStrLn  "Manual transformations:"
  putStr "programs match" >> mapM compare_norm ps >>= print
  putStrLn "Float transformations"
  putStr "programs match" >> mapM compare_float ps >>= print


comparePrint :: (FilePath -> IO CoreProgram) -> FilePath -> FilePath -> IO ()
comparePrint compile fp1 fp2 = do
  p1 <- readFile fp1
  p2 <- readFile fp2
  cp1' <- compile fp1
  let cp1 = removeModInfo cp1'
  cp2' <- compile fp2
  let cp2 = removeModInfo cp2'
  banner "student prog source:"
  putStrLn p1
  printHoleLoc cp1
  banner "model prog source: "
  putStrLn p2
  printHoleLoc cp2
  putStrLn "student prog compiled:"
  print cp1
  putStrLn "model prog compiled:"
  print cp2
  putStrLn $ "Programs match: " ++ show (cp1 ~== cp2)

compare_ :: (FilePath -> IO CoreProgram) -> FilePath -> FilePath -> IO Bool
compare_ comp_pass fp1 fp2 = do
  cp1' <- comp_pass fp1
  let cp1 = removeModInfo cp1'
  cp2' <- comp_pass fp2
  let cp2 = removeModInfo cp2'
  return (cp1 ~== cp2)


compare_desugar, compare_simpl, compare_norm, compare_float :: (FilePath,FilePath) -> IO Bool
compare_desugar = uncurry $ compare_ compC
compare_simpl   = uncurry $ compare_ compS
compare_norm    = uncurry $ compare_ compN
compare_float   = uncurry $ compare_ compF


-- different transformations

compEtaREd fp = ghcToIO $ do 
        (p,e) <- compCoreSt False fp 
        return (etaReduce p,e)


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
      return  $ filter isFile $ map (folderPath </>) dircontent ++ concat subFiles
    else
      return []

isFile :: FilePath -> Bool
isFile path = not (null (takeExtension path))


getAllFiles = getFilePaths path

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


