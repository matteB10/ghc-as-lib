module Transform.Eta where 

import GHC.Core.Utils (exprType)
import GHC.Core
    ( Bind(..), Expr(..), CoreProgram, CoreBind, Alt (..), CoreExpr ) 
import GHC.Types.Var ( Var, isTyVar )
import GHC.Core.Type ( isLinearType ) 
import GHC.Core.Predicate ( isEvVar ) 


import Utils.Utils ( ins )
import Data.Generics.Uniplate.Data ( rewriteBi ) 
import qualified Data.Map as Map 
import GHC.Types.Name (getSrcSpan)
import Data.Maybe (fromJust)
import GHC.Core.Opt.Arity (exprArity)

etaReduce :: CoreProgram -> IO CoreProgram
-- | eta reduction, e.g., \x -> f x => f
etaReduce = return . rewriteBi etaRed
    where etaRed :: CoreExpr ->  Maybe CoreExpr
          etaRed (Lam v (Tick t (App f args)))         | isEtaReducible f args v = return (Tick t f)
          etaRed (Lam v (App (Tick t f) args))         | isEtaReducible f args v = return (Tick t f)
          etaRed (Lam v (App f args))                  | isEtaReducible f args v = return f
          etaRed (Lam v (Let b (Tick t (App f args)))) | isEtaReducible f args v = return (Let b (Tick t f))
          etaRed (Lam v (Let b (App f args)))          | isEtaReducible f args v = return (Let b f)
          etaRed _ = Nothing


isEtaReducible :: CoreExpr -> CoreExpr -> Var -> Bool
isEtaReducible f arg v = case arg of
            Var v' | v == v'
              ,not (isTyVar v)
              ,not (isLinearType (exprType f)) -- don't eta-reduce eta-expanded data constructors (with linear types)
              ,not (ins v f)     -- don't eta reduce if variable used somewhere else in the expression 
              ,not (isEvVar v)   -- don't remove evidence variables 
              -> True
            Tick _ e -> isEtaReducible f e v
            _ -> False


{-

-- eta-expansion, based on GHCs implementation using arity-information
-- not appropriate for programs with holes, since arity 0 by default

etaExpP :: CoreProgram -> Ghc CoreProgram
etaExpP p = do
    env <- getSession
    dflags <- getSessionDynFlags
    let ic = hsc_IC env
        inscopeVars = interactiveInScope ic
        is = mkInScopeSet $ mkUniqSet inscopeVars
    return $ goB dflags p
    where goB :: DynFlags -> CoreProgram -> CoreProgram
          goB df = transformBi $ \bi -> case bi :: CoreBind of
                b@(NonRec v e) -> NonRec v (eta e df)
                b@(Rec es) -> Rec (etaExp df es)
--
          eta expr df = let arit = exprEtaExpandArity df expr
                        in etaExpandAT arit expr
          etaExp df [] = []
          etaExp df ((v,e):es) = (v,eta e df):etaExp df es

-- Taken from GHC, not exported in 9.2.5
-- in previous version, exprIsExpandable would be the equivalent, but returns true in more cases. 
-- these cases are removed here since a lot of expansions would immediately get reduced again,
-- if also performing eta-reduction.
wantEtaExpansion :: DynFlags -> CoreExpr -> Bool
-- Mostly True; but False of PAPs which will immediately eta-reduce again
-- See Note [Which RHSs do we eta-expand?]
wantEtaExpansion df (Cast e _)             = wantEtaExpansion df e
wantEtaExpansion df (Tick _ e)             = wantEtaExpansion df e
wantEtaExpansion df (Lam b e)              = wantEtaExpansion df e
wantEtaExpansion df (App e _)              = wantEtaExpansion df e
wantEtaExpansion _ (Var v)                 = False
wantEtaExpansion _ (Lit {})                = False
wantEtaExpansion df ex@(Let b e)           = exprArity e < arityTypeArity id_arity
    where  id_arity = findRhsArity df (getBindTopVar b) ex (exprArity ex)
wantEtaExpansion _ _                        = True

wantEtaExpB :: DynFlags -> Var -> CoreExpr -> Bool
wantEtaExpB df v e = id_arity > 0
    where id_arity = arityTypeArity $ findRhsArity df v e ex_arity
          ex_arity = exprArity e

-}