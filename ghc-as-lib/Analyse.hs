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
import GHC.Plugins
import Data.Data.Lens (uniplate)
import Data.List (nub)
import Instance (BiplateFor)
import Data.Generics.Biplate (biplateList)



hasRedundantPattern :: CoreProgram -> CoreProgram -> Bool  
-- | Check if student program contains more case alternatives than the closest model solution
--  we can use this together with testing, to determine if we have redundant patterns   
hasRedundantPattern mp sp = 
                    go mpCases spCases
    where mpCases = [c | c@(Case e _ _ alts) <- universeBi mp :: [CoreExpr]]
          spCases = [c | c@(Case e _ _ alts) <- universeBi sp :: [CoreExpr]]
          go :: [CoreExpr] -> [CoreExpr] -> Bool  
          go (m:mc) (s:sc) = checkCase m s
          go [] (s:sc) = True -- no case in model solution
          go _ _       = False 
          checkCase e1 e2 = case (e1,e2) of
            (Case e1 _ _ alts1,Case e2 _ _ alts2) | e1 ~== e2 -> redAlts alts1 alts2  
                                                  | otherwise -> False 
            (_,_) -> False 
          redAlts :: [Alt Var] -> [Alt Var] -> Bool  
          redAlts ms ss = length ms < length ss 
          

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
          redAlts (x:xs) (y:ys) | x ~== y   = redAlts xs ys
                                | otherwise = checkCase (getExpr x) (getExpr y)
          checkCase e1 e2 = case (e1,e2) of
            (Case e1 _ _ alts1,Case e2 _ _ alts2) | e1 ~== e2 -> redAlts alts1 alts2
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


