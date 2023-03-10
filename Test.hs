{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test where


import GHC.Core (CoreProgram)
import Compile
import Transform
import Similar
import Utils


test :: ExerciseName -> IO ()
-- | Main test-function,
-- takes the name of the exercise as a string 
test n = do
  testSuccess n
  testFailure n
  testEta n 

testEta :: ExerciseName -> IO () 
testEta n = do 
  putStrLn "Test eta-reduction desugar"
  mapM (uncurry compare_desugar) etatest >>= print 
  putStrLn "Test eta-reduction simplifier"
  mapM (uncurry compare_simpl) etatest >>= print 
  putStrLn "Test eta-reduction normalised"
  mapM (uncurry (compare_norm n)) etatest >>= print 

testSuccess :: ExerciseName -> IO ()
testSuccess n = do
  putStrLn "Test success desugar:"
  mapM (uncurry compare_desugar) succtest >>= print 
  putStrLn "Test success simpl:"
  mapM (uncurry compare_simpl) succtest >>= print 
  putStrLn "Test success desugar normalised:"
  mapM (uncurry (compare_norm n)) succtest >>= print 

testFailure :: ExerciseName -> IO ()
testFailure n = do
  putStrLn "Test Failure desugar:"
  mapM (uncurry compare_desugar) failtest >>= print 
  putStrLn  "Test Failure simpl:"
  mapM (uncurry compare_simpl) failtest >>= print 
  putStrLn  "Test Failure desugar normalised:"
  mapM (uncurry (compare_norm n)) failtest >>= print 



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

compare_ :: (FilePath -> IO CoreProgram) -> (CoreProgram -> CoreProgram) -> FilePath -> FilePath -> IO Bool
compare_ comp_pass transf fp1 fp2 = do
  cp1' <- comp_pass fp1
  let cp1 = transf (removeModInfo cp1')
  cp2' <- comp_pass fp2
  let cp2 = transf (removeModInfo cp2')
  return $ cp1 ~== cp2


compare_desugar, compare_simpl :: FilePath -> FilePath -> IO Bool
compare_desugar = compare_ (compCore False) id
compare_simpl = compare_ (compSimpl False) id
compare_norm :: String -> FilePath -> FilePath -> IO Bool
compare_norm fname = compare_ (compCore False) (alpha fname . applymany etaRed)


s1, s2, s3, s4, s5, s6 :: Pair
f1, f2 :: Pair
t1 :: Pair

type Pair = (String,String)

pair :: String -> String -> Pair
pair x y = (x,y)


-- DUPLI TESTS  
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

