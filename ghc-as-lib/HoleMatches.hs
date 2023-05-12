module HoleMatches where


import GHC.Core
import GHC.Plugins (getOccString, SrcSpan, RealSrcSpan)
import Data.Maybe
import GHC.Types.Tickish (GenTickish(..))
import Data.Generics.Uniplate.Data (universe)
import Utils (getAltExp)


getHoleMatches :: CoreProgram -> CoreProgram -> [CoreExpr]
-- | Get a list of core expressions matching a hole
--  The student program is expected to be the first one 
getHoleMatches [] [] = []
getHoleMatches (b:bs) (c:cs) = getHoleMs b c ++ getHoleMatches bs cs

getHoleMs :: CoreBind -> CoreBind -> [CoreExpr]
getHoleMs (Rec es) (Rec es')            = concatMap (uncurry match) (zip (map snd es) (map snd es'))
getHoleMs (NonRec v e) (NonRec v' e')   = match e e'
getHoleMs (NonRec v e) (Rec [(v',e')])  = match e e' 
getHoleMs _ _                           = []

match :: CoreExpr -> CoreExpr -> [CoreExpr]
match e1 e2 | isHolev e1 = [e2]
match (App e a) (App e' a') = match e e' ++ match a a'                                       
match (Lam b e) (Lam b' e') = match e e' 
match (Case e v t as) (Case e' v' t' as') = match e e' ++ concatMap (uncurry match) (zip (map getAltExp as) (map getAltExp as'))
match (Let b e) (Let b' e')               = getHoleMs b b' ++ match e e'
match (Tick ct e) e'                      = match e e' 
match e (Tick ct' e')                     | isHolev e = [Tick ct' e']
                                          | otherwise = match e e'               
match _ _                                 = []
 
 
isHolev (Var var)  = take 4 (getOccString var) == "hole"
isHolev (Tick t e) = isHolev e 
isHolev _ = False 

getSrcSpans :: CoreExpr -> Maybe [RealSrcSpan]
getSrcSpans e = return [span | (Tick (SourceNote span _) _) <- universe e :: [CoreExpr]]

getSrcSpan :: CoreExpr -> Maybe RealSrcSpan 
getSrcSpan (Tick (SourceNote span s) e) = Just span 
getSrcSpan _ = Nothing 

