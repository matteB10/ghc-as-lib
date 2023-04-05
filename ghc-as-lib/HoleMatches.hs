module HoleMatches where


import GHC.Core
import GHC.Plugins (getOccString)
import Data.Maybe

getHoleMatches :: CoreProgram -> CoreProgram -> [[CoreExpr]]
-- | Get a list of core expressions 
getHoleMatches [] [] = []
getHoleMatches (b:bs) (c:cs) = getHoleMs b c : getHoleMatches bs cs

getHoleMs :: CoreBind -> CoreBind -> [CoreExpr]
getHoleMs (Rec es) (Rec es')          = concatMap (uncurry match) (zip (map snd es) (map snd es'))
getHoleMs (NonRec v e) (NonRec v' e') = match e e'
getHoleMs _ _                         = []

match :: CoreExpr -> CoreExpr -> [CoreExpr]
match e1 e2 | isHolev e1 && isHolev e2 = []
            | isHolev e1 = [e2]
            | isHolev e2 = [e1]
match (App e a) (App e' a') = match e e' ++ match a a'                                       
match (Lam b e) (Lam b' e') = match e e' 
match (Case e v t as) (Case e' v' t' as') = match e e'  -- should check alternatives as well  
match (Let b e) (Let b' e')               = getHoleMs b b' ++ match e e'
match _ _                                 = []
 
 
isHolev (Var var) = take 4 (getOccString var) == "hole"
isHolev _ = False 