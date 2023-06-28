module CoreLint where 

import Compile.Compile 
import Utils.File (getFilePaths)

import GHC
import GHC.Core 
import GHC.Core.Lint ( lintPassResult )
import GHC.Driver.Env ( HscEnv(..) ) 
import GHC.Core.Opt.Monad
    ( liftIO,
      CoreToDo(..) ) 

import Control.Monad (unless)
import System.Directory (makeAbsolute)

testLintAll :: FilePath -> IO ()
-- | CoreLint all .hs files in a given directory  
testLintAll path = do
    absPath <- makeAbsolute path 
    files <- getFilePaths absPath
    compiled <- mapM (compTestNorm "") files
    mapM_ tcCore compiled
    putStrLn $ "All " ++ show (length compiled) ++ " tests typechecked"

tcCore :: (CoreProgram, HscEnv) -> IO ()
tcCore = uncurry typeCheckCore
   
typeCheckCore :: CoreProgram -> HscEnv -> IO ()
-- | Use the Core Linter to typecheck Core programs 
typeCheckCore coreprog env = do
   let coretodo = CoreDoPasses [CoreDoNothing]
       dflags = hsc_dflags env   
   unless (gopt Opt_DoCoreLinting dflags) $ error "CoreLinting flag must be set"
   liftIO $ lintPassResult env coretodo (coreprog)

