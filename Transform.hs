
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform where

import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt (..), valArgCount, CoreBind)
import GHC.Types.Name ( mkOccName, getOccString, mkInternalName, isDataConName, isSystemName)
import qualified GHC.Types.Name.Occurrence as Occ 
import GHC.Types.Var (Var(..), isTyVar, tyVarKind, isTcTyVar, isId, isTyCoVar, mkLocalVar, idInfo, idDetails, Id, isGlobalId, mustHaveLocalBinding, setVarType, mkGlobalVar)
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Core.Type (eqType)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.Unique 

import Debug.Trace ( trace )
import Instance
import GHC.Core.TyCon (TyCon)
import Control.Monad (when)
import Data.Maybe (isNothing, isJust)
import Similar
import GHC.Types.SrcLoc (mkGeneralSrcSpan, srcLocSpan)
import GHC.Types.Id.Info (IdDetails(VanillaId), vanillaIdInfo, pprIdDetails)
import Control.Monad.Trans.State
import qualified Data.Map as Map 
import Data.Map (Map(..), insert, lookup)
import GHC.Plugins (IdEnv, showSDocUnsafe, Literal (LitString))
import GHC.Base ((<|>))
import GHC.Data.Maybe (fromJust)
import GHC.Utils.Outputable (Outputable(ppr))
import GHC.Iface.Ext.Types (pprIdentifier)
import GHC.Types.Var (isLocalVar, setVarType)
import GHC.Tc.Utils.TcType (isTyConableTyVar)

import qualified Data.Text as T 

import Data.Generics.Uniplate.Data 
import GHC.Types.Literal (Literal)
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Data.Bag (Bag)
import GHC.Types.SrcLoc (GenLocated)
import GHC.Parser.Annotation (SrcSpanAnnA)
import GHC.Hs (GhcTc, XUnboundVar)
import GHC.Hs.Binds (HsBindLR)
import GHC (HsExpr (..))
import GHC.Tc.Types.Evidence (HoleExprRef(..))
import Data.Data (Data)

import Utils 

type Uq = (Char , Int)


data St = St {
        env  :: Map.Map Var Var,
        freshNum  :: Int,
        freshHNum :: Int,
        exerName :: String,
        fits :: Map.Map Var [Expr Var]
        }


type Ctx a = State St a 


newVar :: Id -> Ctx Id
newVar id = do
    --un@(v,i) <- gets freshV
    --nm@(n,j) <- gets freshN 
    --let uq = getUnique id 
    --modify $ \s -> s { freshUniq = 1 } -- update variable supply
    let uq@(c,i) = unpkUnique $ getUnique id 
    j <- gets freshNum
    let name = "n_"++show j
    let id' = makeVar id name
    modify $ \s -> s {env = Map.insert id id' (env s), freshNum = j+1} -- update map 
    return id' 

newHoleVar :: Id -> Type -> Ctx Id 
newHoleVar id t = do 
    let uq@(c,i) = unpkUnique $ getUnique id 
    j <- gets freshHNum
    let name = "hole_"++ show j
    let id' = setVarType (makeVar id name) t
    modify $ \s -> s {env = Map.insert id' id' (env s), freshHNum = j+1} -- update map 
    return id' 


makeGlobVar :: Id -> String -> Id 
makeGlobVar id n = mkGlobalVar id_det name typ id_inf
        where uq     = getUnique id 
              id_det = idDetails id 
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              typ    = varType id 
              id_inf = idInfo id 

    

makeVar :: Id -> String -> Id 
makeVar id n = mkLocalVar id_det name mult typ id_inf
        where uq     = getUnique id 
              id_det = idDetails id 
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = varType id 
              typ    = varType id 
              id_inf = idInfo id 

{- addVar :: Id -> Ctx Id 
addVar id = do 
            let uq@(c,i) = unpkUnique $ getUnique id
            modify $ \s -> s {env = Map.insert id uq (env s)}
            return id -}


alpha :: CoreProgram -> String -> CoreProgram
alpha cs fname = evalState cs' (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fname}) 
    where cs' = mapM alphaR cs  


alphaR :: CoreBind -> Ctx CoreBind
alphaR (NonRec v e) = do 
        v' <- renameVar v
        NonRec v' <$> aRename e 
alphaR (Rec es) = do 
        vars <- mapM (renameVar . fst) es
        exps <- mapM (aRename . snd) es 
        return $ Rec (zip vars exps) 

renameVar :: Id -> Ctx Id 
renameVar v = --trace ("var:" ++ show v ++ " is tyvar: " ++ show (isTyVar v) ++ " musthavebind: " ++ show (mustHaveLocalBinding v) ++ " islocal: " ++ show (isLocalVar v) ++ " isSysName "++show (isSystemName (varName v))) 
            do 
            env <- gets env 
            name <- gets exerName
            case Map.lookup v env of 
                Just n  -> return n  
                Nothing -> checkNew v name  
    where checkNew v n | getOccString v == n = return v 
                       | isTyVar v    = return v 
                       | isTyCoVar v  = return v   ------ this is a problem in the simplifier pass, since local helper gets global ids
                                                   ------ but we dont want to rename global id's like (:)
                       | otherwise    = newVar v 
                
           
aRename :: Expr Var -> Ctx (Expr Var)
aRename (Var id)       = Var <$> renameVar id 
aRename t@(Type _)     = return t 
aRename l@(Lit _)      = return l 
aRename (App e arg)    = do 
        e' <- aRename e 
        arg' <- aRename arg 
        return $ App e' arg' 
aRename l@(Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar b 
        e' <- aRename e 
        return $ Lam b' e'
aRename c@(Case e v t a) | isHoleExpr c  = do 
                            let id = fromJust (getTypErr e)
                            id' <- newHoleVar id t 
                            --trace ("\n hole type equals case type:\n" ++ "vartype: " ++ (showSDocUnsafe $ ppr (varType id')) `nl` "caseT:" ++ ( showSDocUnsafe $ ppr t)) $ 
                            return $ Var id' 
                         | otherwise = do 
                            e' <- aRename e 
                            v' <- renameVar v
                            a' <- renameAlt a 
                            return $ Case e' v' t a'  
aRename (Cast e co)    = do 
        e' <- aRename e 
        return $ Cast e' co 
aRename (Let b e)      = do 
    b' <- alphaR b 
    e' <- aRename e 
    return $ Let b' e'  
aRename (Tick ct e)    = aRename e >>= \e' -> return (Tick ct e')
aRename x              = return x 


-- data Alt Var = Alt AltCon [Var] (Expr Var)
renameAlt :: [Alt Var] -> Ctx [Alt Var]
renameAlt = mapM renameAlt' 
    where renameAlt' :: Alt Var -> Ctx (Alt Var) 
          renameAlt' (Alt ac vs e) = do 
            vs' <- mapM renameVar vs 
            e' <- aRename e 
            return $ Alt ac vs' e' 

 

getTypErr :: Expr Var -> Maybe Id
getTypErr (Var id)       | getOccString id == "typeError" = Just id 
getTypErr (App e arg)    = getTypErr e <|> getTypErr arg
getTypErr (Lam b e)      = getTypErr e 
getTypErr (Case e _ _ _) = getTypErr e 
getTypErr (Let b e)      = getTypErrB b <|> getTypErr e
getTypErr x              = Nothing 

getTypErrB :: CoreBind -> Maybe Id 
getTypErrB (NonRec _ e) = getTypErr e
getTypErrB (Rec es) = case filter isNothing k of 
                    [] -> Nothing 
                    l  -> Just (fromJust $ head l)
        where k = map (getTypErr . snd) es




class ReplaceHoles a where
    repHoles :: a -> a


instance ReplaceHoles CoreProgram where
    repHoles = map repHoles

instance ReplaceHoles (Bind Var) where
    repHoles (Rec es)          = Rec $ zip (map (repHoles . fst) es) (map (repHoles . snd) es)
    repHoles b@(NonRec v e)    | isVarMod v  = b
                               | otherwise =  NonRec (repHoles v) (repHoles e)

instance ReplaceHoles (Expr Var) where
    repHoles (Var id)       = trace ("information from real vars: " ++ show id ++ show (varType id) ++ show (pprIdDetails (idDetails id)))
        Var $ repHoles id 
    repHoles t@(Type _)     = t
    repHoles l@(Lit _)      = l
    repHoles (App e arg)    = App (repHoles e) (repHoles arg)
    repHoles (Lam b e)      = Lam (repHoles b) (repHoles e)
    repHoles c@(Case _ _ t _) | isHoleExpr c  = Var (mkLocalVar id_det name mult typ id_inf)
                            where id_det = VanillaId
                                  name   = mkInternalName (mkUnique 'h' 1) (
                                                          mkOccName Occ.varName "HOLE") 
                                                          (mkGeneralSrcSpan (mkFastString "Hole loc"))
                                  mult   = t
                                  typ    = t
                                  id_inf = vanillaIdInfo
    repHoles (Cast e co)    = repHoles e
    repHoles (Let b e)      = repHoles e 
    repHoles x              = x

instance ReplaceHoles Var where
    repHoles v  = v 
               -- etc  

remOutLam :: CoreProgram -> CoreProgram 
remOutLam = map removeOuterLam
    where removeOuterLam :: Bind Var -> Bind Var    
          removeOuterLam (NonRec v (Lam b e)) = NonRec v e 
          removeOuterLam b                    = b  


removeModInfo :: CoreProgram -> CoreProgram 
removeModInfo = concatMap removeModInf  
    where removeModInf :: Bind Var -> [Bind Var]
          removeModInf (NonRec v e) | isVarMod v = [] 
          removeModInf b                      = [b]


eta :: CoreProgram -> CoreProgram
eta = map etaRed 

etaRed :: Bind Var -> Bind Var 
etaRed (NonRec v e) = NonRec v $ etaRedE e  
    where etaRedE :: Expr Var -> Expr Var 
          etaRedE ex@(Lam v e) | v `ins` e = etaRedE (remove v e)
                               | otherwise = Lam v (etaRedE e) 
          etaRedE ex           = ex                                  
etaRed r          = r 
    
remove :: Var -> Expr Var -> Expr Var                                
remove v (App e (Var v')) | v == v' = e
remove v ex@(App e arg)   | v `ins` arg = App e (remove v arg)  
                          | otherwise   = ex  
remove v (Lam b e)      | v == b = trace ("this should never happen") e 
                        | otherwise = Lam b (remove v e) 
remove v e              = e  

ins :: Var -> Expr Var -> Bool 
ins v (Var v')       = varName v == varName v' && not (isTyVar v) -- varName equality is based on their uniques 
ins v (App e arg)    = ins v arg && not (ins v e)
ins v (Lam b e)      = ins v e
ins v (Case e _ _ _) = ins v e 
ins v (Cast e co)    = ins v e 
ins v (Let b e)      = ins v e 
ins v _              = False 





findHoles :: CoreProgram -> Ctx CoreProgram 
findHoles = mapM findHoles' 

findHoles' :: Bind Var -> Ctx (Bind Var)
findHoles' (NonRec b exp)  = findHole exp >>= \e -> return $ NonRec b e  
findHoles' (Rec es)        = do 
    exs <- mapM (findHole . snd) es
    let vs = map fst es 
    return $ Rec (zip vs exs)

{-Case (App (App (App (Var typeError)
(Type (TyConApp BoxedRep [TyConApp Lifted []])))
(Type (TyConApp () []))) (Lit dupli/Dupli.hs:7:9: e-}

findHole :: Expr Var -> Ctx (Expr Var) 
findHole (Case (App (App (App (Var te) _) _) (Lit emsg)) _ _ t) = do 
        b' <- newHoleVar te (varType te) 
        --fits <- parseFits emsg 

        return $ Var b'
{- findHole v@(Var var) | getOccString (varName var) == "typeError" = do 
                        v' <- newHoleVar var (varType var)
                        return $ Var v'
                     | otherwise = return v  -}
{- findHole (Lit l)            = when (isTypedHolErrMsg l) $ let ((r,c), t) = holeTypeFromMsg (show l)
                                                          in putStrLn $ "found hole at " ++ p (r ++ ":" ++ c) ++ " with type: " ++ t
findHole (App e bndr)       = findHole e >> findHole bndr
findHole (Lam b e)          = findHole e
findHole (Let bi e)         = findHoles' bi >> findHole e
findHole (Case e _ _ alts)  = findHole e >> mapM_ (\(Alt _ _ e) -> findHole e) alts --alts :: Alt AltCon [b] (Expr b)
findHole (Cast  e _)        = findHole e
findHole (Tick _ e)         = findHole e
findHole (Type t)           = return ()
findHole (Coercion c)       = return () -}
findHole e                  = return e 

{- parseFits :: Literal -> Ctx [Var] 
parseFits (LitString s) = do 
        holeType <- holeTypeFromMsg s 
parseFits _             = undefined -}


isTypedHolErrMsg :: Literal -> Bool
isTypedHolErrMsg (LitString l) = let msg = lines $ utf8DecodeByteString l
                                 in last msg == "(deferred type error)"
isTypedHolErrMsg _ = False


holeTypeFromMsg :: String -> Maybe (String, [String])
holeTypeFromMsg s | length s <= 1  = Nothing 
                  | otherwise = Just (htype, vf) 
        where (l1:l2:ls) = lines s 
              rm         = drop 1 (dropWhile (/= ':') l1)
              row        = takeWhile (/= ':') rm
              col        = takeWhile (/= ':') (drop (length row + 1) rm)
              hname      = takeWhile (/= ' ') (drop 2 (dropWhile (/= ':') l2)) -- if hole suffixed with identifier
              htype      = drop 3 $ dropWhile (/= ':') (drop 1 (dropWhile (/= ':') l2))
              (vf,rf)    = retrieveFits ls 

retrieveFits :: [String] -> ([String],[String])
retrieveFits [] = ([],[])
retrieveFits ss = (map T.unpack fits, map T.unpack reffits) 
    where clean = map (T.strip . T.pack) ss 
          rem  = drop 1 $ dropWhile (/= "Valid hole fits include") clean  
          fits = takeWhile (/= "Valid refinement hole fits include") rem 
          reffits = takeWhile (/= "(deferred type error)mainTest") (drop (length fits + 2) rem )
