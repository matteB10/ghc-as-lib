module HoleMatches (getMatches, getSrcSpan) where


import GHC.Core
import GHC.Plugins (getOccString, SrcSpan, RealSrcSpan, mkRealSrcSpan, mkGeneralSrcSpan, mkFastString)
import Data.Maybe
import GHC.Types.Tickish (GenTickish(..), CoreTickish)
import Data.Generics.Uniplate.Data (universe)
import Control.Monad 
import Utils (getAltExp)
import Control.Monad.Trans.State
import GHC.Utils.Monad (concatMapM)
import GHC.Tc.Types.Constraint (Hole)
import GHC (la2r, noSrcSpan, realSrcSpan)


getMatches :: CoreProgram -> CoreProgram -> [(RealSrcSpan, CoreExpr)]
getMatches p1 p2 = let matches = getHoleMatches p1 p2 
                       spans   = map getSrcSpan matches 
                    in zip (catMaybes spans) matches 

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
match (Tick ct e) e'                      = match e e' 
match e (Tick ct' e')                     | isHolev e = [Tick ct' e']
                                          | otherwise = match e e' 
match (App e a) (App e' a') = match e e' ++ match a a'                                       
match (Lam b e) (Lam b' e') = match e e' 
match (Case e v t as) (Case e' v' t' as') = match e e' ++ concatMap (uncurry match) (zip (map getAltExp as) (map getAltExp as'))
match (Let b e) (Let b' e')               = getHoleMs b b' ++ match e e'              
match _ _                                 = []

{- data HoleMatch = HM {exps :: [CoreExpr], tick :: CoreTickish}

getMatches :: CoreProgram -> CoreProgram -> [CoreExpr] 
getMatches s m | null (exps hm) = []
               | otherwise      = let (e:es) = exps hm 
                                      t = tick hm 
                                  in Tick t e : es 
    where hm = execState (getHoleMatches s m) HM {exps = [], tick=dumb_tick} 
          dumb_tick = SourceNote (realSrcSpan noSrcSpan) "dummy"
          

getHoleMatches :: CoreProgram -> CoreProgram -> State HoleMatch () 
-- | Get a list of core expressions matching a hole
--  The student program is expected to be the first one 
getHoleMatches [] [] = return () 
getHoleMatches (b:bs) (c:cs) = getHoleMs b c >> getHoleMatches bs cs

getHoleMs :: CoreBind -> CoreBind -> State HoleMatch () 
getHoleMs (Rec es) (Rec es')            = mapM_ (uncurry match) (zip (map snd es) (map snd es'))
getHoleMs (NonRec v e) (NonRec v' e')   = match e e'
getHoleMs (NonRec v e) (Rec [(v',e')])  = match e e' 
getHoleMs _ _                           = return () 



match :: CoreExpr -> CoreExpr -> State HoleMatch () 
match (Tick t e) (Tick t' e')             = modify (\s -> s {tick = t'}) >> match e e' 
match (Tick t e) e'                       = match e e' 
match e ex@(Tick t' e')                   | isHolev e = modify (\s -> s {tick = t', exps = ex : exps s}) 
                                          | otherwise = modify (\s -> s {tick = t'}) >> match e e' 
match (App e a) (App e' a')               = match e e' >> match a a'                                       
match (Lam b e) (Lam b' e')               = match e e' 
match (Case e v t as) (Case e' v' t' as') = match e e' >> mapM_ (uncurry match) (zip (map getAltExp as) (map getAltExp as'))
--match (Let b e) (Let b' e')               = getHoleMs b b' <> match e e'
match e1 e2 | isHolev e1                  = modify $ \s -> s {exps = e2 : exps s}              
match _ _                                 = return ()  -}

isHolev (Var var)  = take 4 (getOccString var) == "hole"
isHolev (Tick t e) = isHolev e 
isHolev _ = False 

getSrcSpan :: CoreExpr -> Maybe RealSrcSpan
getSrcSpan e = case [span | (Tick (SourceNote span _) _) <- universe e :: [CoreExpr]] of 
        []    -> Nothing 
        (s:_) -> return s 

{- getSrcSpan :: CoreExpr -> Maybe RealSrcSpan 
getSrcSpan (Tick (SourceNote span s) e) = Just span 
getSrcSpan _ = Nothing  -}

