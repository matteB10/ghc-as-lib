{-# LANGUAGE FlexibleInstances #-}
module Feedback.PrintHoleMatch where

import GHC
import Language.Haskell.GHC.ExactPrint ( exactPrint, ExactPrint )
import Data.Generics.Uniplate.Data (universeBi)
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Feedback.HoleMatches (getHsExprFromLoc, getHsRhsFromLoc, getLocalDeclarations)
import Utils.Utils
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.List ( intersperse )
import Debug.Trace (trace)
import Text.Replace
import qualified Data.Text.Lazy as Lazy
import Data.Text (pack)

showParsed :: ExactPrint ast => ast -> String
-- | exact-print parsed source and strip whitespace
showParsed = strip . exactPrint 


showTransParsedFromLoc :: Map String String -> ParsedSource -> RealSrcSpan -> (String, [String])
showTransParsedFromLoc vars ps rss = (trexps,trlocal)
      where trexps = translateStr vars (fst parsed)
            trlocal = map (translateStr vars) (snd parsed)
            parsed = showParsedFromLoc ps rss

showParsedFromLoc :: ParsedSource -> RealSrcSpan -> (String,[String])
-- | Exact print expression on given location
showParsedFromLoc ps@(L l hsm) rss = case getHsExprFromLoc rss ps of -- if we find a matching expression, print it
             Just exp -> (showParsed exp, map showParsed (getLocalDeclarations ps [exp]))
             Nothing   -> case getHsRhsFromLoc rss ps of -- otherwise, we look for the right hand side of the function binding
                  Just d -> (showParsed d, map showParsed (getLocalDeclarations ps [d]))
                  Nothing  -> ("",[])

showTransExp :: ExactPrint ast => Map String String -> ast -> String 
-- | Show translated parsed source 
showTransExp vars parsed = translateStr vars (showParsed parsed)


translateStr :: Map String String -> String -> String
-- | Translate a string using variables from model solutions to student defined variables 
translateStr names expstr = Lazy.unpack (replaceWithTrie trie (Lazy.pack expstr))
      where trie = mapToTrie (listToMap (map (\(k,v) -> Replace (t k) (pack v)) (Map.toList names)))
            t = text'fromString

