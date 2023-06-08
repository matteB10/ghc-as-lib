{-# LANGUAGE FlexibleInstances #-}
module Print where

import GHC
import Language.Haskell.GHC.ExactPrint ( exactPrint, ExactPrint )
import Data.Generics.Uniplate.Data (universeBi)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Utils (getHsExprFromLoc, getHsRhsFromLoc, strip, getLocalDeclarations, rstrip, lstrip)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.List ( intersperse )
import Debug.Trace (trace)
import Text.Replace
import qualified Data.Text.Lazy as Lazy
import Data.Text (pack)
--import Feedback (HoleMatch)

showPsBinds :: HsBindLR GhcPs GhcPs -> String
showPsBinds = strip . exactPrint

showParsedAST :: ParsedSource -> String
-- | Exact print the whole parsed source 
showParsedAST = exactPrint

showParsedExpr :: LHsExpr GhcPs -> String
-- | Exact print a parsed expression 
showParsedExpr = strip . exactPrint


showTransParsedFromLoc :: Map String String -> ParsedSource -> RealSrcSpan -> (String, [String])
showTransParsedFromLoc vars ps rss = (trexps,trlocal)
      where trexps = translateStr vars (fst parsed)
            trlocal = map (translateStr vars) (snd parsed)
            parsed = showParsedFromLoc ps rss

showParsedFromLoc :: ParsedSource -> RealSrcSpan -> (String,[String])
-- | Exact print expression on given location
showParsedFromLoc ps@(L l hsm) rss = case getHsExprFromLoc rss ps of -- if we find a matching expression, print it
             Just exp -> (showParsedExpr exp, map showPsBinds (getLocalDeclarations ps [exp]))
             Nothing   -> case getHsRhsFromLoc rss ps of -- otherwise, we look for the right hand side of the function binding
                  Just d -> (showParsedExpr d, map showPsBinds (getLocalDeclarations ps [d]))
                  Nothing  -> ("",[])


translateStr :: Map String String -> String -> String
translateStr names expstr = Lazy.unpack (replaceWithTrie trie (Lazy.pack expstr))
      where trie = mapToTrie (listToMap (map (\(k,v) -> Replace (t k) (pack v)) (Map.toList names)))
            t = text'fromString

showTransExp :: ExactPrint ast => Map String String -> ast -> String 
showTransExp vars parsed = translateStr vars (strip . exactPrint $ parsed)