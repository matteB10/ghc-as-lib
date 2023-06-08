module Feedback.HoleMatches where


import GHC.Core
import GHC.Plugins (getOccString, SrcSpan, RealSrcSpan, mkRealSrcSpan, mkGeneralSrcSpan, mkFastString)
import Data.Maybe
import GHC.Types.Tickish (GenTickish(..), CoreTickish)
import Data.Generics.Uniplate.Data (universe, universeBi)
import Control.Monad 
import Utils.Utils (getAltExp, nonEmpty, showGhcUnsafe)
import Control.Monad.Trans.State
import GHC.Utils.Monad (concatMapM)
import GHC.Tc.Types.Constraint (Hole)
import GHC (GenLocated(..), RealSrcSpan, ParsedModule, Ghc, la2r, noSrcSpan, realSrcSpan, ParsedSource, SrcSpanAnnA, HsModule (..))
import GHC.Hs ( HsLocalBindsLR (HsValBinds), LIdP, HsBindLR, GhcPs, LHsExpr, HsExpr (..), getLocA)
import GHC.Hs.Binds
import GHC.Hs.Expr
import Data.Data (Data)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)


getMatchLocs :: CoreProgram -> CoreProgram -> [RealSrcSpan]
-- | Get the locations for all found hole-matches
getMatchLocs p1 p2 = mapMaybe getSrcSpan (getHoleMatches p1 p2)

getHoleMatches :: CoreProgram -> CoreProgram -> [CoreExpr]
-- | Get a list of core expressions matching a hole
--  The program containing holes is expected to be the first argument 
getHoleMatches [] [] = []
getHoleMatches (b:bs) (c:cs) = getHoleMs b c ++ getHoleMatches bs cs

getHoleMs :: CoreBind -> CoreBind -> [CoreExpr]
getHoleMs (Rec es) (Rec es')            = concatMap (uncurry match) (zip (map snd es) (map snd es'))
getHoleMs (NonRec v e) (NonRec v' e')   = match e e'
getHoleMs (NonRec v e) (Rec [(v',e')])  = match e e' 
getHoleMs _ _                           = []


match :: CoreExpr -> CoreExpr -> [CoreExpr]
match e1 e2 | isHolev e1                  = [e2]
match (Tick _ (Tick _ e)) (Tick _ (Tick _ e')) = match e e'  -- if nested ticks on both sides we want to look deeper
match (Tick ct e) e'                      = match e e' 
match e (Tick ct' e')                     | isHolev e = [Tick ct' e']
                                          | otherwise = match e e' 
match (App e a) (App e' a')               = match e e' ++ match a a'                                       
match (Lam b e) (Lam b' e')               = match e e' 
match (Case e v t as) (Case e' v' t' as') = match e e' ++ concatMap (uncurry match) (zip (map getAltExp as) (map getAltExp as'))
match e (Case e' v t as)                  = concatMap ((e `match`) . getAltExp) as
match (Let b e) (Let b' e')               = getHoleMs b b' ++ match e e'
match e (Let (Rec es) ine)                = concatMap ((e `match`) . snd) es           
match _ _                                 = []

isHolev (Var var)  = take 4 (getOccString var) == "hole"
isHolev _ = False 


getSrcSpan :: CoreExpr -> Maybe RealSrcSpan 
-- | Get source span from outermost Tick
getSrcSpan (Tick (SourceNote span s) e) = Just span 
-- ideally this should return Nothing 
getSrcSpan e = case [span | (Tick (SourceNote span _) _) <- universe e :: [CoreExpr]] of 
        []    -> Nothing 
        (s:_) -> return s  

getLocalDeclarations :: ParsedSource -> [LHsExpr GhcPs] -> [HsBindLR GhcPs GhcPs]
-- | get local declarations matching a variable
getLocalDeclarations ps@(L l hsm) exps = locDecls
    where rhsVars  = [lidp | ex@(L l (HsVar _ lidp)) <- universeBi exps :: [LHsExpr GhcPs]]
          valBinds = [b | b@(ValBinds {}) <- universeBi (hsmodDecls hsm) :: [HsValBindsLR GhcPs GhcPs]]
          locDecls = [fb | fb@(FunBind ext id' matches _) <- universeBi valBinds :: [HsBindLR GhcPs GhcPs], showGhcUnsafe id' `elem` map showGhcUnsafe rhsVars] -- should be replaced by getName or similar 

getHsVars :: RealSrcSpan -> [LHsExpr GhcPs] -> [LIdP GhcPs]
-- | Get all variables in the RHS of a given source span and list of expressions
getHsVars rss exps = vars
    where rhs = getHsRhs (mapMaybe (matchRealSpan rss) exps)
          vars = [lidp | ex@(L l (HsVar _ lidp)) <- universeBi rhs :: [LHsExpr GhcPs]]


getHsExprFromLoc :: RealSrcSpan -> ParsedSource -> Maybe (LHsExpr GhcPs)
getHsExprFromLoc rss ps@(L l hsm) | nonEmpty locExps = return (head locExps) -- cannot have several expressions on same loc
                                  | otherwise        = Nothing
    where exps      = universeBi ps :: [LHsExpr GhcPs]
          locExps = catMaybes $ filter isJust $ map (matchRealSpan rss) exps
          --locDecls  = [b | (ValBinds _ b _) <- universeBi (hsmodDecls hsm) :: [HsValBindsLR GhcPs GhcPs]]


getHsRhsFromLoc :: RealSrcSpan -> ParsedSource -> Maybe (LHsExpr GhcPs)
getHsRhsFromLoc rss ps@(L l hsm) | nonEmpty locExDec = return (head locExDec)
                                 | otherwise         = Nothing
    where locDecls = mapMaybe (matchRealSpan rss) (hsmodDecls hsm)
          locExDec = getHsRhs locDecls

getHsRhs :: Data a => a -> [LHsExpr GhcPs]
getHsRhs decls = [c | ex@(L loc (GRHS a b c)) <- universeBi decls :: [GenLocated SrcSpan (GRHS GhcPs (LHsExpr GhcPs))]]

getHsMultiLine ::  RealSrcSpan -> ParsedSource -> [HsBindLR GhcPs GhcPs]
getHsMultiLine rss (L l hsm) = locDecls 
    where valBinds = [b | b@(ValBinds {}) <- universeBi (hsmodDecls hsm) :: [HsValBindsLR GhcPs GhcPs]]
          locDecls = [fb | fb@(FunBind ext id' matches _) <- universeBi valBinds :: [HsBindLR GhcPs GhcPs]] -- should be replaced by getName or similar 


matchRealSpan :: RealSrcSpan -> GenLocated SrcSpanAnnA a -> Maybe (GenLocated SrcSpanAnnA a)
matchRealSpan rss ex = case srcSpanToRealSrcSpan (getLocA ex) of
              (Just realspan) | realspan == rss -> Just ex
              _                                 -> Nothing

{- substHs :: Map String String -> LHsExpr GhcPs -> LHsExpr GhcPs
-- not implemented yet, but would be nice with a function to rename parsed
-- expressions instead of "translating" strings later
substHs names = rewriteBi $ \e -> case e :: LHsExpr GhcPs of
        L l (HsVar a b) -> return $ L l (HsVar a b) -- this function does absolutely nothing right now
        ex              -> return ex                -- must figure out how to rename HsVars  -}