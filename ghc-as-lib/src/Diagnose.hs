module Diagnose where 

import Control.Monad (unless, when)
import System.FilePath
import System.Directory

import Compile.Compile (compile, parseExerciseTypSig, checkTypeSig)
import Feedback.Feedback (Feedback(..), mkFeedback)
import Utils.File (writeInput, getFilePaths)
import Utils.String (nl)

type ExercisePath = String 
type StudentInput = String 


diagnose :: StudentInput -> ExercisePath -> FilePath -> IO Feedback 
-- | Return feedback for a given student program
diagnose studentInput exercise tmpFile = do
    path <- makeAbsolute tmpFile
    writeInput path studentInput 
    let exercisename = takeBaseName exercise
    modelFiles <- getFilePaths (modelsPath ++ exercise)
    mInfs <- mapM (compile exercisename) modelFiles -- list of compilation results from model solutions
    when (null modelFiles) $ error ("could not read modelfiles from" ++ exercise)
    hasTypSig <- checkTypeSig path exercisename -- check if student has included a type signature
    typsig <- parseExerciseTypSig (head modelFiles) exercisename 
    unless hasTypSig (writeInput path (typsig `nl` studentInput)) -- rewrite file if student omitted type signature
    stInf <- compile exercisename path  -- compil student program
    return $ mkFeedback stInf mInfs 
     

modelsPath :: FilePath
-- relative paths
modelsPath = "modelsolutions/"
