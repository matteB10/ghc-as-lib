{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

module Feedback where

import Warning ( Warning(..) )
import GHC.Driver.Flags (WarnReason(..), WarningFlag (..))
import GHC.Utils.Outputable (showSDocUnsafe)
import GHC.Core (CoreProgram)
import HoleMatches (getMatches, getSrcSpan)
import Compile (CompInfo(..))
import Similar
import Print
import Data.Maybe (isJust, catMaybes, fromJust)
import Data.List ((\\), deleteFirstsBy, find, isPrefixOf, intercalate)
import GHC.Driver.Ppr (showSDoc)
import Analyse (hasRedundantPattern, missingBaseCase)
import Diff ((~~))
import qualified Data.Map as Map
import Utils
import GHC.Types.Var (Var)
import GHC.Plugins (getOccString)
import GHC (ParsedSource, RealSrcSpan, GenLocated (L), LHsExpr, GhcPs)
import Data.Map (Map)

data Feedback = Complete [Suggestion]
              | Ontrack [Suggestion]
              | Unknown [Suggestion]


data Suggestion = IncompletePat String
                | OverlappingPat String
                | RedundantPat String
                | HLint String
                | HoleSuggestions String
                | HoleMatches [HoleMatch]
                | Error String
                | General String

data HoleMatch = Exp String | LocalFun HoleMatch [HoleMatch] | NoMatch -- maybe something like this?

instance Show Feedback where
  show (Complete f)        = "You've finished the exercise\n" ++ intercalate "\n" (map show f)
  show (Unknown f)         = "Could not match with model solution\n" ++ intercalate "\n" (map show f)
  show (Ontrack f)         = "Solution is on track:\n" ++ intercalate "\n" (map show f)

instance Show Suggestion where
  show (IncompletePat s)   = "You have an incomplete pattern:\n" ++ s
  show (OverlappingPat s)  = "You have an overlapping pattern:\n" ++ s 
  show (RedundantPat s)    = "You might have declared a redundant pattern"
  show (Error s)           = "Detected an error:\n" ++ s
  show (General s)         = s
  show (HoleSuggestions s) = "" -- Holefits needs to be checked, future work  
  show (HoleMatches s)     = showHolMatches s
  show (HLint s)           = "HLint found simplification: " ++ s 

showHolMatches :: [HoleMatch] -> String
showHolMatches xs = intercalate "\n" $ map printHM xs
  where printHM NoMatch = ""
        printHM lf@(LocalFun NoMatch e) = "Found hole, perhaps introduce a local function: " ++ show lf 
        printHM x =  "Found hole, perhaps use: " ++ show x  

suggestions :: Feedback -> [Suggestion]
suggestions (Complete s) = s
suggestions (Ontrack s)  = s
suggestions (Unknown s)  = s

instance Show HoleMatch where
  show (Exp s)               = s
  show (LocalFun NoMatch es) = unwords (map show es)
  show (LocalFun e es)       = show e ++ "\n where\n" ++ unwords (map show es)
  show NoMatch               = ""

mkFeedback :: CompInfo -> [CompInfo] -> Feedback
mkFeedback s ms | isSimilarTo (~=) s ms             = Complete warnfb -- add HLint warnings maybe?
                | isSimilarTo (~>) s ms
                , not (hasOverlappingPat s)
                , not (hasUnmatchedModelWarns s ms) = Ontrack $ bcfb ++ typefb ++ warnfb ++ holefb -- if overlapping, we cannot know if student on track or not
                | isSimilarTo (~>) s ms
                , hasUnmatchedModelWarns s ms       = Unknown $ bcfb ++ typefb ++ warnfb ++ holefb
                | otherwise                         = Unknown $ bcfb ++ typefb ++ warnfb
    where m = getClosest s ms
          warnfb = mkWarnFeedback (warns s) (warns m)
          holefb = mkHoleFeedback' s m
          typefb = mkTypSigFeedback s m
          bcfb   = mkMissingBCFeedback s


mkWarnFeedback :: [Warning] -> [Warning] -> [Suggestion]
mkWarnFeedback ws wms = concatMap getwarnings ws ++ mkModFeedback ws wms
  where getwarnings w
          | w `notElem` wms = case w of
          (GhcWarn (Reason Opt_WarnIncompletePatterns) _ _ doc)  -> [IncompletePat (showSDocUnsafe doc)]
          (GhcWarn (Reason Opt_WarnOverlappingPatterns) _ _ doc) -> [OverlappingPat (showSDocUnsafe doc)]
          (GhcWarn (ErrReason e) _ _ doc)                        -> [Error (showSDocUnsafe doc)]
          (GhcWarn (Reason Opt_WarnTypedHoles) _ _ doc)          -> [HoleSuggestions (showSDocUnsafe doc)]
          (GhcWarn (Reason Opt_WarnDeferredTypeErrors) _ _ doc)  -> [Error (showSDocUnsafe doc)]
          (GhcWarn _ _ _ doc)                                    -> [HLint (showSDocUnsafe doc)] --HLint warnings
          | otherwise = []

mkModFeedback ::[Warning] -> [Warning] -> [Suggestion]
-- | Make feedback from warns (in the model solution) not present in the student solution
mkModFeedback sws mws = concatMap go (mws \\ sws)
  where go (GhcWarn (Reason Opt_WarnIncompletePatterns) _ _ doc)  = [General "The exercise is a partial function, however your solution is defined on all possible input"]
        go (GhcWarn _ _ _ doc)                                    = [] -- don't care about other warns in model solutions 


mkTypSigFeedback :: CompInfo -> CompInfo -> [Suggestion]
-- | Compare type signatures of the exercise function
mkTypSigFeedback s m | hasTypSig ex ps
                     , not $ mainTypeSigMatches ex ps pm =
        [Error ("The given type signature " ++
                 showGhcUnsafe (getSigs ps) ++
                 " does not match the specified type of the exercise " ++
                 showGhcUnsafe (getSigs pm))]
                     | otherwise = [] -- if lacks signature we will probably run into other type errors 
              where ps = parsed s
                    pm = parsed m
                    ex = exercise s

mkMissingBCFeedback :: CompInfo -> [Suggestion]
mkMissingBCFeedback s = case missingBaseCase (core s) of
        Nothing -> []
        Just [v] -> [IncompletePat $ "You might have a missing base case for function " ++ showVar v s]
        Just vs  -> map (\x -> IncompletePat $ "You might have a missing base case for function " ++ showVar x s) vs

showVar :: Var -> CompInfo -> String
showVar v s = case Map.lookup v (keysToVals (names s)) of 
    Just v' -> getOccString v' 
    Nothing -> getOccString v 


isSimilarTo :: (CoreProgram -> CoreProgram -> Bool) -> CompInfo -> [CompInfo] -> Bool
isSimilarTo f student models = any ((core student `f`) . core) models


{- mkHoleFeedback :: CompInfo -> CompInfo -> [Suggestion]
-- | Make hole match feedback 
mkHoleFeedback sp mp = map (HoleMatches . mkMsg) holematch_expr
  where
        holematch_expr    = filter ((""/=) . fst) (printHoleMatch sp mp) -- from model solution
        mkMsg (ex,[])     = ex
        mkMsg (ex,locals) = ex ++ " where\n\t" ++ intercalate "\n" locals -}


mkHoleFeedback' :: CompInfo -> CompInfo -> [Suggestion]
mkHoleFeedback' s m = case getMatches (core s) (core m) of
        [] -> []
        xs -> [HoleMatches (mkSugg xs)]
    where vars = translateNames (names s) (names m)
          mkSugg = map (mkMatches (parsed m) vars . fst)

mkMatches :: ParsedSource -> Map String String -> RealSrcSpan -> HoleMatch
-- | Exact print expression on given location
mkMatches ps@(L l hsm) vars rss = case getHsExprFromLoc rss ps of -- if we find a matching expression, print it
             Just exp | not (isLocFun exp ps) -> Exp (showTransExp vars exp)
                      | otherwise       -> LocalFun (Exp (showTransExp vars exp)) $ map (Exp . showTransExp vars) (getLocalDeclarations ps [exp])
             Nothing   -> case getHsRhsFromLoc rss ps of -- otherwise, we look for the right hand side of the function binding
                  Just d | not (isLocFun d ps) -> Exp (showTransExp vars d)
                         | otherwise       -> LocalFun (Exp (showTransExp vars d)) $ map (Exp . showTransExp vars) (getLocalDeclarations ps [d])
                  Nothing  -> case getHsMultiLine rss ps of
                    [] -> NoMatch
                    f  -> LocalFun NoMatch $ map (Exp . showTransExp vars) f

isLocFun :: LHsExpr GhcPs -> ParsedSource -> Bool
isLocFun e ps = not (null (getLocalDeclarations ps [e]))



printHoleMatch :: CompInfo -> CompInfo -> [(String,[String])]
printHoleMatch s m = strings
        where holematches = getMatches (core s) (core m)
              nameMap = translateNames (names s) (names m)
              strings = map (showTransParsedFromLoc nameMap (parsed m)) (map fst holematches)

hasRedundantPat :: CompInfo -> [CompInfo] -> Bool
-- | Check if a student attempt contains a redundant pattern 
hasRedundantPat s m = hasRedundantPattern (core s) (core closest_model)
      where closest_model = getClosest s m

hasOverlappingPat :: CompInfo -> Bool
-- | Check if the warns includes an overlapping pattern warning 
hasOverlappingPat inf = Reason Opt_WarnOverlappingPatterns `elem` map reason (warns inf)

getClosest :: CompInfo -> [CompInfo] -> CompInfo
-- | Get closest matching model solution if it has no matching one 
getClosest s ms | not $ null successors = getsmall s successors
                | otherwise = getsmall s ms
    where getsmall s ms = fst $ foldr1 (minBy snd) $ map (compare s) ms
          compare p1 p2 = (p2, core p1 ~~ core p2)
          successors = filter (\m -> core s ~> core m) ms

minBy :: Ord a => (b -> a) -> b -> b -> b
minBy f x y
  | f x < f y = x
  | otherwise = y

hasUnmatchedModelWarns :: CompInfo -> [CompInfo] -> Bool
-- | Model solution contains warning messages not present in the student solution
hasUnmatchedModelWarns s m = any (\x -> reason x /= NoReason) (warns closest_model \\ warns s)
  where closest_model = getClosest s m

-- Utility functions for feedback 

isMatch :: Feedback -> Bool
isMatch = \case
  Complete _  -> True
  _           -> False

isOnTrack :: Feedback -> Bool
isOnTrack = \case
  Ontrack _ -> True
  _         -> False

isMissingCases :: Feedback -> Bool
isMissingCases f = any isIncPat (suggestions f)
  where isIncPat = \case
          IncompletePat _ -> True
          _               -> False

isOverLapping :: Feedback -> Bool
isOverLapping f = any isOvPat (suggestions f)
  where isOvPat = \case
          OverlappingPat _ -> True
          _                -> False

isUnknown :: Feedback -> Bool
isUnknown =  \case
  Unknown _   -> True
  _           -> False

isHLint :: Feedback -> Bool 
isHLint f = any isHL (suggestions f) 
  where isHL = \case 
          HLint _ -> True 
          _       -> False 

isHoleMatch :: Feedback -> Bool
isHoleMatch f = any isHoleM (suggestions f)
  where isHoleM = \case
          HoleMatches _ -> True
          _             -> False

