{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Instance where
-- | Module containing Show instances for GHC types

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
import GHC.Core (Bind(..), Expr(..), Alt(..), AltCon(..))
import GHC.Unit.Module.Warnings (Warnings(..), pprWarningTxtForMsg)
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
import GHC.Base (leInt)
import GHC.Plugins (IdInfo, showSDoc)
import GHC.IORef (IORef)
import Data.Void (Void)
-- ===============

deriving instance Show HoleFitPlugin
deriving instance Show Hole
deriving instance Show HoleSort

instance Show FitPlugin where
  show fp = "FitPlugin"

instance Show Implication where
  show i = "Implication"

instance Show OccName where
  show n = showSDocUnsafe $ pprOccName n

instance Show CandPlugin where
  show c = "CandPlugin"

instance Show HoleExprRef where
  show (HER e t u) = "(HER " ++ "evTerm " ++ "type: " ++ show t  ++ " uq: " ++ show u ++ ")"

instance Show CtLoc where
  show c = "CtLoc"

{- instance Show VarSet where 
  show x = showSDocUnsafe (pprVarSet x)  -}

instance Show a => Show (UniqSet a) where
  show x = showSDocUnsafe (pprUniqSet undefined x)

{- instance Show Cts where
  show c = ""  -}


instance Show Var where
  --show x = showSDocUnsafe $ ppr (varName x)  -- show uniques aswell
  show = getOccString

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
  show t = "CoercionR "

deriving instance Show Type

instance Show AnonArgFlag where
  show aaf = "AnonArgFlag"

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
  show c = "coretickish "

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
  show x = ""

instance Show ConLike where
  show c = ""

instance Show DynFlags where
  show (DynFlags {generalFlags = g}) = show g


instance Show (EnumSet GeneralFlag) where
  show e = intercalate ", \n" (map show (toList e))


-- =========== SHOW INSTANCE FOR HSSYN

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
deriving instance Show SrcSpanAnnA
deriving instance Show (HsMatchContext (GhcPass 'Renamed))
deriving instance Show (HsStmtContext (GhcPass 'Renamed))
deriving instance Show (GhcPass 'Renamed)
deriving instance Show a => Show (EpAnn a)
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
deriving instance Show GhcPs

instance Show (HsDecl GhcPs) where show a = showSDocUnsafe $ ppr a

-- belongs to HsDecl if trying to derive instance 
{- instance Show (TyClDecl GhcPs) where show a = ""
instance Show (FamilyDecl GhcPs) where show a = ""
instance Show (InstDecl GhcPs) where show a = ""
instance Show (DerivDecl GhcPs) where show a = ""
instance Show (DefaultDecl GhcPs) where show a = ""
instance Show a => Show (HsBind a) where show a = ""
instance Show (SpliceDecl GhcPs) where show a = ""
instance Show (RuleDecls GhcPs) where show a = ""
instance Show (AnnDecl GhcPs) where show a = ""
instance Show (WarnDecls GhcPs) where show a = ""
instance Show (ForeignDecl GhcPs) where show a = ""
instance Show (StandaloneKindSig GhcPs) where show a = "" -}

instance Show (SrcSpanAnn' (EpAnn AnnList)) where show a = ""

instance Show EpAnnComments where
  show a = showSDocUnsafe $ ppr a

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



instance Show a => Show (HsOverLit a) where
  show s = ""

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
  show s = ""
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

