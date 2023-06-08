module Annotation where 

import GHC.Core ( CoreExpr, CoreProgram, Expr(Tick) ) 
import GHC
    ( ParsedSource,
      RealSrcSpan,
      GhcPs,
      LHsExpr,
      GhcTc,
      GenLocated(..),
      SrcSpanAnnA,
      HsExpr(HsApp, HsTick, HsUnboundVar, HsVar),
      NoExtField(NoExtField),
      SrcSpanAnn'(SrcSpanAnn),
      la2r,
      realSrcSpan,
      isGoodSrcSpan,
      TypecheckedSource ) 
import GHC.Types.Tickish ( CoreTickish, GenTickish(SourceNote) )
import GHC.Data.Bag ( bagToList )


import Data.Generics.Uniplate.Data

getTickishSrcSpan :: CoreTickish -> RealSrcSpan 
getTickishSrcSpan (SourceNote span _) = span 

getTickLocations :: CoreProgram -> [RealSrcSpan]
getTickLocations p = map getTickishSrcSpan (getTicks p) 

extractTicksT :: TypecheckedSource -> [CoreTickish] -- [GenLocated SrcSpanAnnA (HsExpr GhcTc)]
extractTicksT b = ticks
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            ticks = [n | L l (HsTick _ n a) <- exps]

getTicks :: CoreProgram -> [CoreTickish]
getTicks bs = [t | (Tick t a) <- universeBi bs :: [CoreExpr]]

extractTicksP :: ParsedSource -> [CoreTickish]--[GenLocated SrcSpanAnnA (HsExpr GhcPs)]
extractTicksP bs = ticks
      where 
            exps =  universeBi bs :: [GenLocated SrcSpanAnnA (HsExpr GhcPs)]
            ticks = [n |  ex@(L l (HsTick _ n a)) <- exps]

extractUn :: ParsedSource -> [LHsExpr GhcPs]
extractUn bs = ticks
      where 
            exps =  universeBi bs :: [LHsExpr GhcPs]
            ticks = [ex | ex@(L loc (HsUnboundVar tg n)) <- exps]

mkNote :: String -> RealSrcSpan -> CoreTickish
mkNote s span = SourceNote span s 
 
tickNote :: String -> LHsExpr GhcTc -> LHsExpr GhcTc
tickNote msg e@(L loc@(SrcSpanAnn an sp) ex) = L loc (HsTick NoExtField (mkNote msg rss) e)
  where rss = realSrcSpan sp  

attachNote :: TypecheckedSource -> TypecheckedSource
attachNote = fmap attach
 where
  attach = transformBi $ \e -> case e :: GenLocated SrcSpanAnnA (HsExpr GhcTc) of
    ex@(L loc (HsTick _ _ e)) -> tickNote "TICK" e 
    ex@(L loc (HsApp {}))  | isRealLocation loc -> tickNote "app" ex 
    ex@(L loc (HsVar {}))  | isRealLocation loc ->  tickNote "var" ex 
    ex@(L loc (HsUnboundVar {})) | isRealLocation loc -> tickNote "hole" ex 
    ex@(L loc e) | isRealLocation loc -> tickNote "note" ex 
                 | otherwise -> ex 

isRealLocation :: SrcSpanAnnA -> Bool 
isRealLocation (SrcSpanAnn _ span) = isGoodSrcSpan span 

  

 {- where
    extr = transformBi $ \e -> case e :: GenLocated SrcSpanAnnA (HsExpr GhcPs) of
    ex@(L loc e) | la2r loc == srcloc -> ex  -}

{- extractLits :: TypecheckedSource -> [LHsExpr GhcTc]
extractLits b = lits
      where bs = map (\(L _ x) -> x) (bagToList b)
            exps = universeBi bs :: [LHsExpr GhcTc]
            lits = [L l (HsLit li a) | L l (HsLit li a) <- exps] -}
  
attachNoteP :: ParsedSource -> ParsedSource
attachNoteP = fmap attach
 where
  attach = transformBi $ \e -> case e :: GenLocated SrcSpanAnnA (HsExpr GhcPs) of
    --HsApp{} -> tickNoteP "APPI" e 
    ex@(L loc HsUnboundVar{}) | isRealLocation loc -> tickNoteP "hole" ex 
    ex@(L loc HsVar {}) | isRealLocation loc -> tickNoteP "var" ex 
    --HsUnboundVar{} -> tickNoteP "hole" e 
    ex@(L loc e) | isRealLocation loc -> tickNoteP "general" ex  
                 | otherwise -> ex 

tickNoteP :: String -> LHsExpr GhcPs -> LHsExpr GhcPs
tickNoteP msg e@(L loc ex) = L loc (HsTick NoExtField (mkNoteP msg rss) e)
  where rss = la2r loc  

mkNoteP :: String -> RealSrcSpan -> CoreTickish
mkNoteP s span = SourceNote span s