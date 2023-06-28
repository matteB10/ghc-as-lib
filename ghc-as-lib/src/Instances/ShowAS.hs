{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Instances.ShowAS where
-- | Module containing Show instances for GHC types
--  CoreSyn and HsSyn 

import GHC
import GHC.Core.DataCon (DataCon(..), mkDataCon, dataConName)
import GHC.Types.Var (Var(..), TyCoVarBinder(..), VarBndr(..), ArgFlag(..), AnonArgFlag, TyCoVar, Specificity(..))
import GHC.Types.Literal (Literal(..), LitNumType, pprLiteral)
import GHC.Types.Basic
    ( FunctionOrData(IsFunction, IsData),
      PromotionFlag(..),
      RecFlag,
      Origin,
      Boxity )
import GHC.Types.Tickish ( CoreTickish )
import GHC.Core.TyCon (TyCon(..))
import GHC.Core.TyCo.Rep (Type(..), CoercionR, TyLit(..), TyCoBinder)
import GHC.Types.Name (Name(..),
                      isHoleName,
                      nameStableString,
                      OccName,
                      getSrcLoc,
                      NamedThing (getName),
                      pprOccName,
                      pprDefinedAt,
                      getOccString,
                      nameUnique,
                      HasOccName (occName),
                      isDataConName,
                      isTyConName,
                      isTyVarName,
                      isSystemName)
import GHC.Tc.Errors.Hole.FitTypes (TypedHole(..), HoleFitPlugin(..), FitPlugin(..), CandPlugin(..))
import GHC.Tc.Types.Constraint (Hole(..), HoleSort(..), Implication, CtLoc, Cts)
import GHC.Tc.Types.Evidence (HoleExprRef (..), HsWrapper, TcEvBinds, QuoteWrapper)
import GHC.Types.Unique.Set (UniqSet, pprUniqSet)
import GHC.Core (Bind(..), Expr(..), Alt(..), AltCon(..), CoreProgram, CoreBind, CoreExpr)
import GHC.Unit.Module.Warnings (Warnings(..), pprWarningTxtForMsg, WarningTxt)
import GHC.Types.TyThing (TyThing(..))
import GHC.Core.Coercion.Axiom (CoAxiom)
import GHC.Core.ConLike (ConLike)
import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Driver.Session (DynFlags(..), GhcNameVersion, PlatformMisc)
import GHC.Unit.Types (GenModule(..), Unit(..))
import GHC.Data.EnumSet (EnumSet, toList)
import GHC.Driver.Flags (GeneralFlag(..))
import Data.List (intersperse, intercalate)

-- == for hsSyn == 
import GHC.Data.Bag (Bag(..), bagToList, unitBag, isEmptyBag)
import Data.Void (Void)
import Data.Generics.Uniplate.Data ( Biplate )
-- ===============

--deriving instance Show HoleFitPlugin
--deriving instance Show Hole
--deriving instance Show HoleSort

{- instance Show FitPlugin where
  show fp = "FitPlugin" -}

instance Show Implication where
  show i = "Implication"

instance Show OccName where
  show n = showSDocUnsafe $ pprOccName n

{- instance Show CandPlugin where
  show :: CandPlugin -> String
  show c = "CandPlugin" -}

instance Show HoleExprRef where
  show (HER e t u) = "(HER " ++ "evTerm " ++ "type: " ++ show t  ++ " uq: " ++ show u ++ ")"

instance Show CtLoc where
  show c = "CtLoc"

{- instance Show VarSet where 
  show x = showSDocUnsafe (pprVarSet x)  -}

instance Show a => Show (UniqSet a) where
  show x = showSDocUnsafe (pprUniqSet undefined x)


instance Show Var where
  --show x = showSDocUnsafe $ ppr (varName x)  -- show uniques aswell
  show = getOccString  -- show occurence name

deriving instance Show b => Show (Bind b)
deriving instance Show b => Show (Expr b)
deriving instance Show b => Show (Alt b)
deriving instance Show AltCon

--deriving instance Show Literal
deriving instance Show TyLit

instance Show Literal where
  show (LitString l)    = utf8DecodeByteString l
  show (LitChar c)      = [c]
  show (LitNumber _ i)  = show i
  show l                = "lit"


instance Show TyCon where
  show t = name2Str $ tyConName t


instance Show CoercionR where
  show = showSDocUnsafe . ppr

deriving instance Show Type

instance Show AnonArgFlag where
  show = showSDocUnsafe . ppr

deriving instance (Show a, Show b) => Show (VarBndr a b)
deriving instance Show ArgFlag
deriving instance Show Specificity

{- instance Show Type where 
  show (TyVarTy v)         = p $ "TyVarTy " ++ show v 
  show (AppTy t1 t2)       = p $ "AppTy " ++ show t1 ++ " " ++ show t2 
  show (TyConApp t tks)    = p $ "TyConApp " ++ show t ++ concatMap show tks 
  show (ForAllTy _ t)      = p $ "ForAllTy " ++ show t 
  show (FunTy _ _ arg res) = p $ "FunTy " ++ show arg ++ " -> " ++ show res 
  show (LitTy tl)          = p $ "LitTy " ++ show tl 
  show (CastTy t _)        = p $ "CastTy " ++ show t 
  show (CoercionTy c)      = p $ "CoercionTy " ++ show c  -}

p :: [Char] -> [Char]
p x = "(" ++ x ++ ")"

instance Show CoreTickish where
  show = showSDocUnsafe . ppr

instance Show FunctionOrData where
  show IsFunction = "(function)"
  show IsData     = "(data)"


instance Show LitNumType where
  show _ = "Int " -- Litnum types are Int/Nat/Words of different byte sizes 


instance Show DataCon where
  --show d = "DCon: " ++ showSDocUnsafe (ppr d)  -- show both name and unique
  show d = if isHoleName (dataConName d) then showSDocUnsafe (ppr d) else showSDocUnsafe (ppr d)
    --name2Str (dataConName d) 


-- show occName (not unique)
name2Str :: Name -> String
name2Str = getOccString

instance Show Warnings where
  show (NoWarnings)  = "No warning"
  show (WarnAll w)   = showSDocUnsafe $ pprWarningTxtForMsg w
  show (WarnSome ws) = concatMap (\(oc,wt) -> show oc ++ showSDocUnsafe (pprWarningTxtForMsg wt)) ws

deriving instance Show TyThing

instance Show (CoAxiom a) where
  show = showSDocUnsafe . ppr

instance Show ConLike where
  show = showSDocUnsafe . ppr

instance Show DynFlags where
  show (DynFlags {generalFlags = g}) = show g


instance Show (EnumSet GeneralFlag) where
  show e = intercalate ", \n" (map show (toList e))


-- =========== SHOW INSTANCE FOR HSSYN ===============================================================

instance Show a => Show (Bag a) where
  show b | isEmptyBag b     = "EmptyBag"
         | otherwise        = show $ bagToList b


deriving instance (Show a, Show b) => Show (GenLocated b a) --- removing this shows an empty list 

deriving instance Show (HsBindLR GhcTc GhcTc)
deriving instance Show (PatSynBind GhcTc GhcTc)
deriving instance Show (HsExpr GhcTc)
deriving instance Show (HsWildCardBndrs (GhcPass 'Renamed) (GenLocated SrcSpanAnnA (HsType (GhcPass 'Renamed))))
deriving instance Show (HsRecField' (FieldLabelStrings GhcTc) (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show GhcTc
deriving instance Show (GRHS GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show (GRHSs GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show (MatchGroup GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show (Match GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show (StmtLR GhcTc GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc)))
deriving instance Show (ParStmtBlock GhcTc GhcTc)
deriving instance Show (HsLocalBindsLR GhcTc GhcTc)
deriving instance Show (HsValBindsLR GhcTc GhcTc)
--deriving instance Show a => Show (MatchGroup GhcTc a)
--deriving instance Show a => Show (Match GhcTc a)
--deriving instance Show a => Show (GRHSs GhcTc a)
deriving instance Show (NHsValBindsLR (GhcPass 'Typechecked))
--deriving instance Show SrcSpanAnnA
deriving instance Show (HsMatchContext (GhcPass 'Renamed))
deriving instance Show (HsStmtContext (GhcPass 'Renamed))
deriving instance Show (GhcPass 'Renamed)
deriving instance Show NoExtField
deriving instance Show NoExtCon
deriving instance Show SrcStrictness
deriving instance Show HsArrowMatchContext
deriving instance Show (FieldLabelStrings GhcTc)
deriving instance Show (HsFieldLabel GhcTc)
deriving instance Show (AnnFieldLabel)
deriving instance Show EpaLocation
deriving instance Show AnnContext
deriving instance Show (HsType (GhcPass 'Renamed))
deriving instance Show PromotionFlag
instance Show (ConDeclField (GhcPass 'Renamed))
deriving instance Show (HsArrow (GhcPass 'Renamed))
deriving instance Show (HsPatSynDir GhcTc)
deriving instance Show (AmbiguousFieldOcc GhcTc)
--deriving instance Show (HsSplice (GhcPass 'Renamed))
--deriving instance Show (HsSplicedThing (GhcPass 'Renamed))
--deriving instance Show (HsExpr (GhcPass 'Renamed))
--deriving instance Show (HsForAllTelescope (GhcPass 'Renamed))
deriving instance Show NoEpAnns
deriving instance Show AnnParen
deriving instance Show ParenType
deriving instance Show HsIPName
deriving instance Show RecordUpdTc
deriving instance Show HsTupleSort
deriving instance Show HsTyLit


instance Show (HsDecl GhcPs) where
  show (TyClD t1 t2) = "TyClD " ++ showO t1 `sp` showO t2      -- ^ Type or Class Declaration
  show (InstD d1 d2) = "InstD " ++ showO d1 `sp` showO d2     -- ^ Instance declaratin
  show (DerivD d1 d2) = "DerivD " ++ showO d1 `sp` showO d2   -- ^ Deriving declaration
  show (ValD v1 v2) = "valD " ++ show v1 `sp` show v2    -- ^ Value declaration
  show (SigD s1 s2) = "SigD " ++ showO s1 `sp` showO s2    -- ^ Signature declaration
  show a = showSDocUnsafe $ ppr a

--deriving instance Show (HsBind GhcPs)
--deriving instance Show (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
--deriving instance Show (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))

instance Show (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where show = showO
instance Show (HsOverLit GhcPs) where show = showO
instance Show (HsPatSigType (GhcPass 'Parsed)) where show = showO
instance Show EpAnnSumPat where show a = ""
instance Show (HsConDetails Void (GenLocated SrcSpanAnnN RdrName) [RecordPatSynField GhcPs]) where show a = ""
instance Show (HsPatSynDir GhcPs) where show a = ""
instance Show (HsRecField' (AmbiguousFieldOcc GhcPs) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where show a = ""
instance Show (HsRecField' (FieldLabelStrings GhcPs) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where show a = ""
instance Show (HsWildCardBndrs
                        (GhcPass 'Parsed)
                        (GenLocated SrcSpanAnnA (HsType (GhcPass 'Parsed)))) where show a = ""
instance Show (HsWildCardBndrs
                        (GhcPass 'Parsed)
                        (GenLocated SrcSpanAnnA (HsSigType (GhcPass 'Parsed)))) where show a = ""
instance Show (AmbiguousFieldOcc GhcPs) where show a = ""
instance Show (ArithSeqInfo GhcPs) where show a = ""
instance Show (HsFieldLabel GhcPs) where show a = ""
deriving instance Show (HsBindLR GhcPs GhcPs)
deriving instance (Show (Pat GhcPs))
deriving instance Show (HsConDetails (HsPatSigType (GhcPass 'Parsed)) (GenLocated SrcSpanAnnA (Pat GhcPs)) (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))))
deriving instance Show (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs)))
instance Show (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (Pat GhcPs))) where show a = ""
instance Show EpAnnUnboundVar where show a = ""
instance Show AnnExplicitSum where show a = ""
instance Show AnnsLet where show a = ""

instance Show AnnsIf where show a = ""
instance Show AnnProjection where show a = ""
instance Show EpAnnHsCase where show a = ""
deriving instance Show (PatSynBind GhcPs GhcPs)
deriving instance Show (HsExpr GhcPs)
deriving instance Show GhcPs
deriving instance Show (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (HsMatchContext (GhcPass 'Parsed))
deriving instance (Show (HsStmtContext (GhcPass 'Parsed)))
deriving instance Show (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (ApplicativeArg GhcPs)
deriving instance Show (ParStmtBlock GhcPs GhcPs)
deriving instance Show (HsLocalBindsLR GhcPs GhcPs)
deriving instance Show (HsValBindsLR GhcPs GhcPs)
deriving instance Show (NHsValBindsLR (GhcPass 'Parsed))
deriving instance Show (HsIPBinds GhcPs)
deriving instance Show (IPBind GhcPs)
instance Show (ABExport GhcPs) where show a = ""

sp s1 s2 = s1 ++ " " ++ s2

showO :: Outputable a => a -> String
showO = showSDocUnsafe . ppr

instance Show a => Show (EpAnn a) where
  show (EpAnn anchor ann comment) = "EpAnn " ++ show ann
  show EpAnnNotUsed               = "EpAnnNotUsed"

instance Show SrcSpanAnnA where
  show (SrcSpanAnn ann span) = show ann

instance Show (SrcSpanAnn' (EpAnn AnnList)) where
  show (SrcSpanAnn n an) = showSDocUnsafe $ ppr n

instance Show EpAnnComments where
  show a = "" -- showSDocUnsafe $ ppr a

instance Show (SrcSpanAnn' (EpAnn AnnContext)) where
  show a = ""


instance Show (HsForAllTelescope (GhcPass 'Renamed)) where
  show a = ""

instance Show HsSrcBang where
  show a = ""


instance Show XXExprGhcTc where
  show a = ""

instance Show PendingTcSplice where
  show a = ""

instance Show QuoteWrapper where
  show a = ""

instance Show PendingRnSplice where
  show a = ""

--deriving instance Show HsIPBinds

instance Show Boxity where
  show a = ""


instance Show a => Show (HsPragE a) where
  show a = ""

instance Show a => Show (HsCmdTop a) where
  show a = ""

instance Show a => Show (HsSplice a) where
  show a = ""

instance Show a => Show (HsBracket a) where
  show a = ""

instance Show (ArithSeqInfo GhcTc) where
  show a = ""

instance Show a => Show (HsTupArg a) where
  show a = ""


instance Show (HsLit a) where
  show (HsChar x c) = show c
  show (HsCharPrim x c) = show c
  show (HsString x fs) = show fs
  show (HsStringPrim x bs) = show bs
  show (HsInt x intLit) = show intLit
  show (HsIntPrim x int) = show int
  show (HsWordPrim x int) = show int
  show (HsInt64Prim x int) = show int
  show (HsWord64Prim x int) = show int
  show (HsInteger x int ty) = show int
  show (HsRat x fracLit ty) = show fracLit
  show (HsFloatPrim x fracLit) = show fracLit
  show (HsDoublePrim x fracLit) = show fracLit
  show (XLit x) = "XLit"



instance Show (HsOverLit GhcTc) where
  show (OverLit a b c) = show b ++ show c

instance Show (OverLitVal) where
   show (HsIntegral   il) = show il            -- ^ Integer-looking literals;
   show (HsFractional fl) = show fl          -- ^ Frac-looking literals
   show (HsIsString   st fs) = show st       -- ^ String-looking literals


instance Show TrailingAnn where
  show a = showSDocUnsafe $ ppr a

instance Show RdrName where
  show r = showSDocUnsafe $ ppr r

instance Show (HsWildCardBndrs (GhcPass 'Renamed) (GenLocated SrcSpanAnnA (HsSigType (GhcPass 'Renamed)))) where
  show a = showSDocUnsafe $ ppr a

instance Show Origin where
  show a = showSDocUnsafe $ ppr a

instance Show RecStmtTc where
  show a = ""
instance Show TransForm where
  show a = ""

instance Show AddEpAnn where
  show a = showSDocUnsafe $ ppr a

instance Show (XBindStmtTc) where
  show a = ""

instance Show (ApplicativeArg GhcTc) where
  show a = showSDocUnsafe $ ppr a

instance Show (HsIPBinds GhcTc) where
  show a = showSDocUnsafe $ ppr a

instance Show AnnSortKey where
  show a = showSDocUnsafe $ ppr a


instance Show LexicalFixity where
  show l = showSDocUnsafe $ ppr l

instance Show Name where
  show = getOccString

instance Show RecFlag where
  show r = showSDocUnsafe $ ppr r

instance Show SyntaxExprTc where
  show s = showSDocUnsafe $ ppr s

instance Show (HsConDetails Void (GenLocated SrcSpanAnnN Id) [RecordPatSynField GhcTc]) where
  show k = ""

instance Show (HsRecField' (AmbiguousFieldOcc GhcTc) (GenLocated SrcSpanAnnA (HsExpr GhcTc))) where
  show k = ""
instance Show (HsRecFields GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc))) where
  show k = ""
instance Show GrhsAnn where
  show s = showSDocUnsafe $ ppr s
instance (Show a) => Show (Sig a) where
  show s = ""

instance Show MatchGroupTc where
  show m = ""


instance Show AnnListItem where
  show a = showSDocUnsafe $ ppr a

instance Show AnnList where
  show a = showSDocUnsafe $ ppr a

instance Show (SrcSpanAnn' (EpAnn NameAnn)) where
  show b = ""

instance Show (Pat GhcTc) where
  show p = showSDocUnsafe $ ppr p

instance Show (ABExport GhcTc) where
  show p = ""

instance Show HsWrapper where
  show h = showSDocUnsafe $ ppr h

instance Show TcEvBinds where
  show t = showSDocUnsafe $ ppr t



---- INSTANCES FOR PARSED SRC
deriving instance Show HsModule

instance Show AnnsModule where show a = ""

instance Show WarningTxt where show a = ""
instance Show (SrcSpanAnn' (EpAnn AnnPragma)) where show a = ""

instance Show (IE GhcPs) where show imp = ""
instance Show (ImportDecl GhcPs) where show i = ""
