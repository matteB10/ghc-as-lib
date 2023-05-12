{-# LANGUAGE LambdaCase #-}

module Feedback where

import Warning ( Warning(..) )
import GHC.Driver.Flags (WarnReason(..), WarningFlag (..))
import GHC.Utils.Outputable (showSDocUnsafe)
import GHC.Core (CoreProgram)
import HoleMatches (getHoleMatches, getSrcSpan)
import Compile (CompInfo(..))
import Similar
import Print (showParsedFromLoc, showTransParsedFromLoc)
import Data.Maybe (isJust, catMaybes, fromJust)
import Data.List ((\\), deleteFirstsBy, find, isPrefixOf)
import GHC.Driver.Ppr (showSDoc)
import Analyse (hasRedundantPattern)
import Diff ((~~))
import Utils

data Feedback = NoWarns
              | Complete Feedback
              | Ontrack Feedback
              | Unknown Feedback
              | IncompletePat String
              | OverlappingPat String
              | RedundantPat String
              | HLint String
              | HoleSuggestions String
              | HoleMatches String
              | Error String
              | General String
              | Many [Feedback]

instance Show Feedback where
  show (Complete f)        = "You've finished the exercise\n" ++ show f
  show NoWarns             = ""
  show (IncompletePat s)   = "You have an incomplete pattern:\n" ++ s
  show (OverlappingPat s)  = "You have an overlapping pattern:\n" ++ s
  show (RedundantPat s)    = "You might have declared a redundant pattern\n"
  show (Error s)           = "Detected an error:\n" ++ s
  show (Unknown f)         = "Could not match with model solution\n" ++ show f
  show (Ontrack f)         = "Solution is on track:\n" ++ show f
  show (General s)         = s
  show (Many fs)           = concatMap show fs
  show (HoleSuggestions s) = "" -- Holefits needs to be checked, future work  
  show (HoleMatches s)     = "Perhaps use " ++ s ++ "\n"
  show (HLint s)           = "HLint found simplification: " ++ s ++ "\n" 

mkFeedback :: CompInfo -> [CompInfo] -> Feedback
mkFeedback s ms | isSimilarTo (~=) s ms             = Complete warnfb -- add HLint warnings maybe?
                | isSimilarTo (~>) s ms 
                , not (hasOverlappingPat s)
                , not (hasUnmatchedModelWarns s ms) = Ontrack $ typefb >+> warnfb >+> holefb -- if overlapping, we cannot know if student on track or not
                | isSimilarTo (~>) s ms 
                , hasUnmatchedModelWarns s ms       = Unknown $ typefb >+> warnfb >+> holefb 
                | otherwise                         = Unknown $ typefb >+> warnfb
    where m = getClosest s ms  
          warnfb = mkWarnFeedback (warns s) (warns m)
          holefb = mkHoleFeedback s m 
          typefb = mkTypSigFeedback s m 
         

mkWarnFeedback :: [Warning] -> [Warning] -> Feedback
mkWarnFeedback ws wms = Many (map getwarnings ws) >+> mkModFeedback ws wms 
  where getwarnings w
          | w `notElem` wms = case w of
          (GhcWarn (Reason Opt_WarnIncompletePatterns) _ _ doc)  -> IncompletePat (showSDocUnsafe doc)
          (GhcWarn (Reason Opt_WarnOverlappingPatterns) _ _ doc) -> OverlappingPat (showSDocUnsafe doc)
          (GhcWarn (ErrReason e) _ _ doc)                        -> Error (showSDocUnsafe doc)
          (GhcWarn (Reason Opt_WarnTypedHoles) _ _ doc)          -> HoleSuggestions (showSDocUnsafe doc)
          (GhcWarn (Reason Opt_WarnDeferredTypeErrors) _ _ doc)  -> Error (showSDocUnsafe doc)
          (GhcWarn _ _ _ doc)                                    -> HLint (showSDocUnsafe doc) --HLint warnings
          | otherwise = NoWarns

mkModFeedback ::[Warning] -> [Warning] -> Feedback
-- | Make feedback from warns (in the model solution) not present in the student solution
mkModFeedback sws mws = Many $ map go (mws \\ sws)
  where go (GhcWarn (Reason Opt_WarnIncompletePatterns) _ _ doc)  = General "The exercise is a partial function, however your solution is defined on all possible input"
        go (GhcWarn _ _ _ doc)                                    = NoWarns -- don't care about other warns in model solutions 


mkTypSigFeedback :: CompInfo -> CompInfo -> Feedback
mkTypSigFeedback s m | hasTypSig ex ps 
                     , not $ mainTypeSigMatches ex ps pm = 
        Error $ "Given type signature " ++ showGhcUnsafe (getSigs ps) ++ " does not match the specified type of the exercise."
                     | otherwise = NoWarns -- if lacks signature we will probably run into other type errors 
              where ps = parsed s  
                    pm = parsed m 
                    ex = exercise s 



isSimilarTo :: (CoreProgram -> CoreProgram -> Bool) -> CompInfo -> [CompInfo] -> Bool
isSimilarTo f student models = any ((core student `f`) . core) models


mkHoleFeedback :: CompInfo -> CompInfo -> Feedback
-- | Make hole match feedback 
mkHoleFeedback sp mp = Many $ map HoleMatches (filter (/= "") holematch_expr)
  where holematch_expr = printHoleMatch sp mp 
        

printHoleMatch :: CompInfo -> CompInfo -> [String] 
printHoleMatch s m = strings 
        where holematches = getHoleMatches (core s) (core m)
              spans = catMaybes $ filter isJust $ map getSrcSpan holematches
              nameMap = translateNames (names s) (names m)
              strings = map (showTransParsedFromLoc nameMap (parsed m)) spans

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
isMissingCases = \case
   Many f          -> any isMissingCases f
   IncompletePat _ -> True
   _               -> False

isOverLapping :: Feedback -> Bool
isOverLapping =  \case
  Many f           -> any isOverLapping f
  OverlappingPat _ -> True
  _                -> False


(>+>) :: Feedback -> Feedback -> Feedback
Many fs >+> Many gs = Many (fs ++ gs)
f >+> g             = Many (f : [g])
