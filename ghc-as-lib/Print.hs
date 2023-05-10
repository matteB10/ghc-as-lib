{-# LANGUAGE FlexibleInstances #-}
module Print where

import GHC
import Language.Haskell.GHC.ExactPrint ( exactPrint )
import Data.Generics.Uniplate.Data (universeBi)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Utils (getHsExprFromLoc, getHsRhsFromLoc)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.List ( intersperse )
import Debug.Trace (trace)

showParsedAST :: ParsedSource -> String
-- | Exact print the whole parsed source 
showParsedAST = exactPrint

showParsedExpr :: LHsExpr GhcPs -> String
-- | Exact print a parsed expression 
showParsedExpr = strip . exactPrint

showTransParsedFromLoc :: Map String String -> ParsedSource -> RealSrcSpan -> String 
showTransParsedFromLoc vars ps rss = translateStr vars (showParsedFromLoc ps rss)

showParsedFromLoc :: ParsedSource -> RealSrcSpan -> String
-- | Exact print expression on given location
showParsedFromLoc ps@(L l hsm) rss = case getHsExprFromLoc rss ps of -- if we find a matching expression, print it
             Just exps -> unwords $ map showParsedExpr exps
             Nothing   -> case getHsRhsFromLoc rss ps of -- otherwise, we look for the right hand side of the function binding
                  Just rhs -> unwords $ map showParsedExpr rhs
                  Nothing  -> ""


translateStr :: Map String String -> String -> String
translateStr names expstr = unwords $ map replace exps
      where exps = map strip $ words expstr
            replace ex = fromMaybe ex (Map.lookup ex names)

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs) | x `elem` chars = lstrip xs
              | otherwise      = x:xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

chars :: String
chars = " \t\r\n"
