{-# LANGUAGE FlexibleInstances #-}
module TransformationTests where

import Data.Generics.Uniplate.Data ( universeBi )

import GHC.Core (CoreExpr, CoreProgram, Bind (..), Expr(..))
import GHC.Core.Utils (exprType)
import GHC.Plugins ( Id, Bind, CoreProgram, CoreExpr, exprType, getOccString )
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))

import Transform
import Compile ( compCore, compNorm, compNormNoRename )
import System.Directory
import System.FilePath
import Data.List (isPrefixOf, isSuffixOf)
import Debug.Trace (trace)

exerc :: [String]
exerc = ["dupli","myreverse","fromBin","factorial"]

folders :: [String]
folders = ["/good/", "/bad/", "/norm/"]


testCoreLint :: IO () 
testCoreLint = do 
    let folderpaths = [e ++ f | e <- exerc, f <- folders]
    filepaths <- mapM con folderpaths
    mapM_ compNormNoRename (concat filepaths)


testAll :: IO ()
testAll = do
    let folderpaths = [e ++ f | e <- exerc, f <- folders]
    filepaths <- mapM con folderpaths
    let ls = concat $ mapM (mapM test folders) (concat filepaths)
    m <- andB ls 
    print m 

andB :: [IO Bool] -> IO Bool 
andB []     = return True 
andB (b:bs) = do 
    b' <- b 
    bs' <- andB bs 
    return $ b' && bs'

con :: FilePath -> IO [FilePath]
con f = do
    files <- listDirectory f 
    let files' = [fd | fd <- files, ".hs" `isSuffixOf` fd]
    return [f ++ file | file <- files]

printIO :: Show a => IO a -> IO ()
printIO a = do
    a' <- a
    print a'

test :: String -> FilePath -> IO Bool
test s fp = do
        p <- compCore False fp
        let p' = rewriteRec s p
        let ep = etaReduce p'
            bs  = [v | v <- universeBi p' :: [Bind Id]] --, getOccString (getBindTopVar v) == s]
            bs' = [v' | v' <- universeBi p' :: [Bind Id]] --, getOccString (getBindTopVar v') == s] --- RETURNS EMPTY
        case bs of
            (NonRec v e):rs -> case bs' of
                (NonRec v' e'):rs' -> return $ proptypePreservedEta e e'
                (Rec es):ls -> trace "found rec" $ return False
                b -> trace ("rec: " ++ show b) $ return False
            (Rec es):ls -> trace "found rec" $ return False
            b -> trace ("rec: " ++ show b) $ return False

proptypePreservedEta :: Expr Id -> Expr Id -> Bool
proptypePreservedEta e1 e2 = exprType e1 == exprType e2




{- 

import Test.QuickCheck  
import GHC.Types.Id (Id)
import GHC.Types.Unique (mkUnique)
import GHC (Type)
import qualified GHC.Types.Name.Occurrence as Occ


Need a more sophisticated idea to construct arbitrary type correct programs 

instance Arbitrary (Bind Id) where 
    arbitrary = do id <- arbitrary :: Gen Id 
                   ls <- arbitrary :: Gen [(Id, Expr Id)]
                   ex <- arbitrary :: Gen (Expr Id )
                   elements [Rec ls, NonRec id ex]

instance Arbitrary (Expr Id) where 
    arbitrary = do 
        id <- arbitrary :: Gen Id 
        ex <- arbitrary :: Gen (Expr Var)
        ex2 <- arbitrary :: Gen (Expr Var)
        frequency [(9, return $ Var id), 
                   (1, return $ Lam id ex),
                   (2, return $ App ex ex2)]

instance Arbitrary Id where 
    arbitrary = do 
        s <- genString 
        uc <- genChar
        ui <- arbitrary :: Gen Int 
        typ <- arbitrary :: Gen Type 
        let id_det = VanillaId 
            uq = mkUnique uc ui 
            id_inf = vanillaIdInfo
            name = mkInternalName uq (mkOccName Occ.varName s) (mkGeneralSrcSpan (mkFastString ("Loc " ++ s)))
        return $ mkLocalVar id_det name typ typ id_inf 

genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Gen String 
genString = listOf genChar



instance Arbitrary Type where -- only type variables
    arbitrary = let name = mkInternalName (mkUnique 'c' 1) (mkOccName Occ.varName "a") (mkGeneralSrcSpan (mkFastString ("Loc " ++ "a")))
                    kind = LitTy $ CharTyLit 'a'
                in return $ TyVarTy (mkTyVar name kind)

genTyLit :: Gen TyLit 
genTyLit = do 
    str <- arbitrary :: Gen String 
    let fstr = return $  mkFastString str 
    oneof [NumTyLit <$> arbitrary,
           StrTyLit <$> fstr,
           CharTyLit <$> arbitrary]


genLit :: Gen Literal 
genLit = oneof [LitChar <$> arbitrary, 
                LitFloat <$> arbitrary,
                LitDouble <$> arbitrary]


mkVanillaVar :: Var
mkVanillaVar =  let id_det = VanillaId 
                    uq = mkUnique 'c' 1  
                    id_inf = vanillaIdInfo
                    name = mkInternalName uq (mkOccName Occ.varName "a") (mkGeneralSrcSpan (mkFastString ("Loc " ++ "a")))
                    typ = undefined  
                in mkLocalVar id_det name typ typ id_inf   -}