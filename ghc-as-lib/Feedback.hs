{-# LANGUAGE LambdaCase #-}

module Feedback where 

import Warning ( Warning(..) ) 
import GHC.Driver.Flags (WarnReason(..), WarningFlag (..))
import GHC.Utils.Outputable (showSDocUnsafe)

data Feedback = Complete
              | NoWarns
              | Ontrack Feedback
              | IncompletePat String
              | OverlappingPat String
              | RedundantPat String 
              | General String
              | HoleSuggestions String
              | Error String
              | Many [Feedback]

instance Eq Feedback where
  Complete == Complete                     = True
  NoWarns == NoWarns                       = True
  (IncompletePat _) == (IncompletePat _)   = True
  (OverlappingPat _) == (OverlappingPat _) = True
  _ == _                                   = False


instance Show Feedback where
  show Complete            = "You've finished the exercise"
  show NoWarns             = ""
  show (IncompletePat s)   = "You have an incomplete pattern:\n" ++ s
  show (OverlappingPat s)  = "You have an overlapping pattern:\n" ++ s
  show (RedundantPat s)    = "You might have declared a redundant pattern"
  show (Error s)           = "Detected an error " ++ s
  show (Ontrack f)         = "Solution is on track\n" ++ show f
  show (Many fs)           = concatMap show fs
  show (HoleSuggestions s) = "Holes could be replaced by:\n" ++ show s
  show (General s)         = s

getFeedback :: [Warning] -> Feedback
getFeedback [] = NoWarns
getFeedback ws = Many $ map getwarnings ws 
  where getwarnings w = case w of
          (GhcWarn (Reason Opt_WarnIncompletePatterns) _ _ doc)  -> IncompletePat (showSDocUnsafe doc)
          (GhcWarn (Reason Opt_WarnOverlappingPatterns) _ _ doc) -> OverlappingPat (showSDocUnsafe doc)
          (GhcWarn (ErrReason e) _ _ doc)                        -> Error (showSDocUnsafe doc)
          (GhcWarn (Reason Opt_WarnTypedHoles) _ _ doc)          -> HoleSuggestions (showSDocUnsafe doc) 
          (GhcWarn _ _ _ doc)                                    -> General (showSDocUnsafe doc)
          


mkFeedback :: Bool -> Bool -> Bool -> [Warning] -> Feedback
mkFeedback match pred redpat warn | redpat = RedundantPat ""
                                  | match  = Complete 
                                  | pred && Reason Opt_WarnOverlappingPatterns `notElem` warnreason = Ontrack (getFeedback warn)
                                  | otherwise = getFeedback warn 
  where warnreason = map reason warn

-- Utility functions for feedback 

isMatch :: Feedback -> Bool
isMatch = \case
  Complete  -> True
  _         -> False

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
