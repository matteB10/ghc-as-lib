{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analyse where

{-# LANGUAGE RankNTypes #-}

import Data.Data
import Control.Lens ( lengthOf, Traversal' )
import Control.Lens hiding (universe, children )



import GHC.Core
import GHC.Types.Var (Var)
import Data.Generics.Uniplate.Data (universe, universeBi, transformBi, rewriteBi, rewriteBiM, transformBiM, Uniplate (descend), Biplate (..), childrenBi, children)
import Similar
import Diff ((~~))
import Utils (getAltExp)
import GHC.Plugins
import Data.Data.Lens (uniplate)
import Data.List (nub)
import Data.Generics.Biplate (biplateList, Biplate)
import Data.Maybe (catMaybes, mapMaybe)



hasRedundantPattern :: CoreProgram -> CoreProgram -> Bool
-- | Check if student program contains more case alternatives than the closest model solution
--  we can use this together with testing, to determine if we have redundant patterns   
hasRedundantPattern mp sp =
                    go mpCases spCases
    where mpCases = [c | c@(Case e _ _ alts) <- universeBi mp :: [CoreExpr]]
          spCases = [c | c@(Case e _ _ alts) <- universeBi sp :: [CoreExpr]]
          go :: [CoreExpr] -> [CoreExpr] -> Bool
          go (m:ms) (s:ss) = checkCase m s || length ms < length ss
          go [] (s:sc) = True -- no case in model solution
          go _ _       = False
          checkCase e1 e2 = case (e1,e2) of
            (Case e1 _ _ alts1,Case e2 _ _ alts2) | e1 ~= e2 -> length alts1 < length alts2
                                                              || not (any (hasCase . getAltExp) alts1) && any (hasCase . getAltExp) alts2
            (_,_) -> False


hasCase :: CoreExpr -> Bool
hasCase e = not $ null [ex | ex@(Case {}) <- universe e, not (e ~= ex)]

hasCaseB :: CoreBind -> Bool
hasCaseB e = not $ null [ex | ex@(Case {}) <- universeBi e :: [CoreExpr]]

getRedundantPattern :: CoreProgram -> CoreProgram -> [Alt Var]
-- | Check if student program contains more case alternatives than the model solution
--  we can use this together with testing, to determine if we have redundant patterns   
getRedundantPattern mp sp = {- if length mpCases == length spCases then zipWith (~==) mpCases spCases
                            else  -}
                    go mpCases spCases
    where mpCases = [c | c@(Case e _ _ alts) <- universeBi mp :: [CoreExpr]]
          spCases = [c | c@(Case e _ _ alts) <- universeBi sp :: [CoreExpr]]
          go :: [CoreExpr] -> [CoreExpr] -> [Alt Var]
          go (m:mc) (s:sc) = checkCase m s ++ go mc sc
          go [] (s:sc) = getAlts s ++ go [] sc
          go _ _   = []
          redAlts :: [Alt Var] -> [Alt Var] -> [Alt Var]
          redAlts [] [] = []
          redAlts [] xs@(y:ys) = xs
          redAlts (x:xs) (y:ys) | x ~= y   = redAlts xs ys
                                | otherwise = checkCase (getExpr x) (getExpr y)
          checkCase e1 e2 = case (e1,e2) of
            (Case e1 _ _ alts1,Case e2 _ _ alts2) | e1 ~= e2 -> redAlts alts1 alts2
            (_,Case _ _ _ a) -> a
            (_,_) -> []



getExpr :: Alt Var -> Expr Var
getExpr (Alt _ _ e) =  e

getAlts :: CoreExpr -> [Alt Var]
getAlts (Case _ _ _ a) = a
getAlts _              = error "no case"



getAllSubExprs :: CoreProgram -> [CoreExpr]
getAllSubExprs (p:ps) = nub (a ++ b ++ c ++ d )
    where a = [e | e@(Lam _ _) <- universeBi p :: [CoreExpr]]
          b = [e | e@(App _ _) <- universeBi p :: [CoreExpr]]
          c = [e | e@(Let b _) <- universeBi p :: [CoreExpr]]
          d = [e | e@(Case {}) <- universeBi p :: [CoreExpr]]
          --f = [e | e@(Case {}) <- universeBi p :: [CoreExpr]]


missingBaseCase :: CoreProgram -> Maybe [Var]
missingBaseCase p = return $ catMaybes $ concatMap missingBase p
      where missingBase (Rec es) = map (\(v,e) -> if not (hasCase e) then Just v else Nothing) es 
            missingBase b = [Just v | (Let (Rec ((v,e):es)) _) <- universeBi b :: [CoreExpr], not (hasCase e)] 