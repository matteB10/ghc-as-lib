{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Inline where 

import GHC (Name, Ghc, HscEnv, GhcMonad (..), getSessionDynFlags, isLocalId, SrcSpan, isPrimOpId)
import GHC.Core
    ( Bind(NonRec, Rec), Expr(Var, Lam, Let), CoreProgram, CoreBind ) 
import GHC.Types.Name ( getOccString )
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Var

 
import Data.ByteString (ByteString)
import GHC.Types.Id ( localiseId ) 
import GHC.Types.Unique.Supply
    ( UniqSupply, listSplitUniqSupply, mkSplitUniqSupply )


import Data.Bifunctor ( Bifunctor(second) )
import Data.Generics.Uniplate.Data
import Data.Maybe ( fromJust )
import Data.ByteString.Char8 (pack)
import Data.List (delete, (\\))
import Utils.Utils
    ( fresh,
      getBindTopVar,
      getBinds,
      isSpecVar,
      isTyConApp,
      makeLocal,
      subst,
      updateVar )
import Instances.Similar
import Control.Monad.Trans.State (State, evalState, get, modify)


inlineBinds :: String -> CoreProgram -> IO CoreProgram
-- | Inline recursive binders as let-recs, and non recursive binders directly
inlineBinds exfun = return . inlineBind []
    where inlineBind :: CoreProgram -> CoreProgram -> CoreProgram
          inlineBind acc [] = acc 
          inlineBind acc (b:bs) | not (isSpecVar v)
                            , exfun /= getOccString v  
                            , length usedBy == 1 
                            , notTyCon $ head usedBy = 
                                let (newBinds, rest) = inline b usedBy (acc ++ bs) 
                                in inlineBind newBinds rest 
                            | otherwise = inlineBind (b:acc) bs
                where v = getBindTopVar b 
                      usedBy = getBinds (acc ++ bs) b
                      notTyCon b = not (isTyConApp . varType . getBindTopVar $ b)
                       
          

inline :: CoreBind -> [CoreBind] -> [CoreBind] -> ([CoreBind],[CoreBind])
-- | Inline a binder and return remaining binders 
inline b inlbs bs = let b'  = updateVar (makeLocal (getBindTopVar b)) b -- change scope to local of binder variable if inlined 
                        bs' = insertBind b inlbs
                    in (bs', delete b (bs \\ inlbs))

insertBind :: CoreBind -> [CoreBind] -> [CoreBind]
-- | Inline binder in all binders using it 
--  recursive binders are inlined as a let-rec
insertBind n@(NonRec v e) bs = map insertB bs -- inline another nonrec 
    where insertB bi@(NonRec b e') = (transformBi $ \case
            (Var v') | v == v' -> e
            e -> e) bi
          insertB bi@(Rec es) = (transformBi $ \case
            (Var v') | v == v' -> e
            e -> e) bi
insertBind (Rec ls@((v,e):es)) bs = map insertR bs
    where insertR bind@(NonRec b (Lam x ex)) = NonRec b $ Lam x $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          insertR bind@(NonRec b ex) = NonRec b $
                                         Let (Rec ((uv,e'):es)) (subst uv v ex)
          insertR bind@(Rec es') = Rec $ map (second (subst uv v)) (es'++ ls)
          uv = localiseId v
          e' = subst uv v e

recToLetRec :: CoreProgram -> IO CoreProgram
-- | Inline recursive binders as let-recs when appropriate
recToLetRec p = mkSplitUniqSupply 'R' >>= \us -> return (evalState (recToLR (listSplitUniqSupply us) p) 0)
    where recToLR :: [UniqSupply] -> CoreProgram -> State Int CoreProgram
          recToLR _ [] = return []
          recToLR us (b:bs) = case b of
            rr@(Rec ((v,e):ls)) -> do
                            i <- get
                            modify $ \s -> s+1 -- state counter for indexing a new unique 
                            let v' = fresh (us !! i) v
                                e' = subst v' v e
                                ls' = map (subst v' v . snd) ls
                                b' = NonRec v (Let (Rec ((v',e'):ls)) (Var v'))
                            recToLR us bs >>= \bs' -> return $ b':bs
            nr@(NonRec v e) -> recToLR us bs >>= \bs' -> return $ nr:bs'