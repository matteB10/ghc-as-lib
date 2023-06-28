module Utils.File where 

import System.IO 
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, makeAbsolute)
import System.FilePath ((</>), takeDirectory, takeExtension, takeBaseName)
import Data.List (isSuffixOf, isPrefixOf, sortOn)
import Data.Char (isDigit)
import Data.Ord
import Data.List ((\\))

import Utils.String 


getFilePaths :: FilePath -> IO [FilePath]
-- get all non config .hs file from a given path
getFilePaths folderPath = do 
    isDir <- doesDirectoryExist folderPath 
    isFile <- doesFileExist folderPath
    go isDir isFile folderPath
    where isConfig fp = "Config.hs" `isSuffixOf` fp
          go isd isf fp | isd = do 
                            dircontent <- listDirectory folderPath
                            subFiles <- mapM getFilePaths [folderPath </> f | f <- dircontent]
                            let content = map (folderPath </>) dircontent ++ concat subFiles
                            return $ filter (\fp -> (".hs" `isSuffixOf` fp) && not (isConfig fp) && isFile fp) content
                        | isf = return [folderPath]
                        | otherwise = return [] 

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


isFile :: FilePath -> Bool
isFile path = not (null (takeExtension path))


rulesFile :: FilePath 
rulesFile  = "src/Rules.hs"

writeInput :: FilePath -> String -> IO ()
writeInput path input = do
    rules <- readFile =<< makeAbsolute rulesFile  
    let inputstr = "module Temp where\n" `nl` input `nl` rules
    handle <- openFile path WriteMode
    hPutStrLn handle inputstr
    hFlush handle
    hClose handle