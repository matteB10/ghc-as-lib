{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Transform.Remove where 

import GHC.Core
    ( Alt(..),
      AltCon(DataAlt),
      Bind(..),
      Expr(..),
      CoreProgram,
      CoreExpr )
import GHC.Types.Id ( Var )
import GHC.Types.Literal ( Literal(LitString) )
import GHC.Types.Name ( getOccString )
import GHC.Core.Predicate (isEvVar)
import GHC.Types.Unique ( Uniquable(getUnique) )
import GHC.Types.SrcLoc ( mkGeneralSrcSpan )
import GHC.Types.Var ( isTyVar, setVarName )

import Data.Data (Data)
import Data.Generics.Uniplate.Data
    ( rewriteBi, transformBi, universe )
import Utils.Utils
    ( getVarFromName,
      isEvOrTyExp,
      isEvOrTyVar,
      isPatError,
      isWild,
      makeName,
      subE,
      subst ) 
import Data.Maybe ( fromJust )
import Data.ByteString.Char8 (pack)



removeRedEqCheck :: CoreProgram -> IO CoreProgram
-- | Remove redundant equality checks
removeRedEqCheck = return . rewriteBi remEqCheck

    where remEqCheck :: CoreExpr ->  Maybe CoreExpr
          -- | remove redundant boolean checks, e.g. 
          -- if x == y then true else false ==> x == y 
          -- f x y | x == y =    True 
          --       | otherwise = False      ==> x == y 
          remEqCheck (Case e v t alt) | isEqCheck e || isNeqCheck e
                                      , all isBoolToBool alt    = Just e
                                      | isEqCheck e
                                      , all isNegBoolToBool alt = Just (replace "==" "/=" e)
                                      | isNeqCheck e
                                      , all isNegBoolToBool alt = Just (replace "/=" "==" e)
          remEqCheck _ = Nothing
          replace :: String -> String -> CoreExpr -> CoreExpr
            -- | Replace the variable name with another name, and 
            --   update all occurences of the variable in the given expression
          replace old new e = subst vnew vold e
            where vold = fromJust $ getVarFromName old e
                  vnew = setVarName vold (makeName new (getUnique vold) (mkGeneralSrcSpan "Dummy loc"))


isEqCheck :: Data Var => CoreExpr -> Bool
isEqCheck e = or [getOccString v == "==" | Var v <- universe e]

isNeqCheck  :: Data Var => CoreExpr -> Bool
isNeqCheck e = or [getOccString v == "/=" | Var v <- universe e]

isBoolToBool :: Alt Var -> Bool
-- | Case on a bool that also returns a bool
isBoolToBool (Alt (DataAlt d) [] (Var v)) = dstr == vstr &&
                                            dstr == "False" || dstr == "True"
        where dstr = getOccString d
              vstr = getOccString v
isBoolToBool _                            = False

isNegBoolToBool :: Alt Var -> Bool
-- | Case on a bool that also returns a bool, but with reversed logic
isNegBoolToBool (Alt (DataAlt d) [] (Var v)) = (dstr == "False" &&
                                                vstr == "True") ||
                                               (dstr == "True"  &&
                                                vstr == "False")
         where dstr = getOccString d
               vstr = getOccString v
isNegBoolToBool _                            = False



removeTyEvidence :: CoreProgram -> CoreProgram
-- | Remove types and type evidence from a Coreprogram
removeTyEvidence = transformBi removeTy

    where removeTy = \case
            (Lam v e)        | isEvVar v || isTyVar v -> e
            (App f (Var v))  | isEvVar v || isTyVar v -> f
            (App f (Type t)) -> f
            (Let b e) | isEvBind b -> e
            e -> e
          isEvBind (NonRec bi e) = isEvOrTyVar bi && isEvOrTyExp e
          isEvBind (Rec es) = all (isEvOrTyVar . fst) es && all (isEvOrTyExp . snd) es


replacePatErrorLits :: CoreProgram -> CoreProgram
-- | Replace patern error literals with "patternError"
replacePatErrorLits = transformBi $ \case
    c@(Case e v t alt) | isPatError c -> Case (rlit e) v t alt
                       | otherwise -> c
    e -> e
    where rlit = transformBi $ \ex -> case ex :: CoreExpr of
            (Lit (LitString s)) -> Lit (LitString (pack "pattern error")) 
            e -> e

-- not used in normalisation, can be used for readability 
removeTicks :: CoreProgram -> CoreProgram
-- | Remove all Tick expressions
removeTicks = rewriteBi remTick
    where remTick :: CoreExpr -> Maybe CoreExpr
          remTick ex = case ex of
            (Tick _ e) -> Just e
            e -> Nothing 

{- removeModInfo :: CoreProgram -> IO CoreProgram
-- | Remove module information
--   is ok to use if only considering exercises with functions
removeModInfo p = return $ concatMap removeModInf p
    where removeModInf :: CoreBind -> [CoreBind]
          removeModInf (NonRec v e) | isVarMod v = []
          removeModInf b                         = [b]
 -}
