{-# LANGUAGE FlexibleInstances #-}
module Print where

import GHC
import Language.Haskell.GHC.ExactPrint ( exactPrint )
import Data.Generics.Uniplate.Data (universeBi)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import Data.Maybe (isJust, catMaybes)

showParsedAST :: ParsedSource -> String
-- | Exact print the whole parsed source 
showParsedAST = exactPrint

showParsedDecl :: LHsDecl GhcPs -> String
-- | Exact print a parsed expression 
showParsedDecl = strip . exactPrint

showParsedExpr :: LHsExpr GhcPs -> String
-- | Exact print a parsed expression 
showParsedExpr = strip . exactPrint

showParsedFromLoc :: ParsedSource -> RealSrcSpan -> String
-- | Exact print expression on given location
showParsedFromLoc = flip extractExprFromSrcLoc

extractExprFromSrcLoc :: RealSrcSpan -> ParsedSource -> String 
extractExprFromSrcLoc srcloc bs@(L l hsm) | null exprs && not (null decls') = concatMap showParsedDecl decls' -- may print whole solution 
                                          | null exprs && null decls' = ""
                                          | otherwise = concatMap showParsedExpr exprs 
  where
        exps = universeBi bs :: [LHsExpr GhcPs]
        decls = hsmodDecls hsm 
        decls' = catMaybes $ filter isJust $ map findRealspans decls 
        exprs = catMaybes $ filter isJust $ map findRealspans exps
        findRealspans ex = case srcSpanToRealSrcSpan (getLocA ex) of
            (Just realspan) | realspan == srcloc -> Just ex
            _               -> Nothing

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
