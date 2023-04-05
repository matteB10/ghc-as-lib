{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where 

import Instance 
import GHC.Core
import GHC.Core.Type (isTyVar)

class PPr a where
    pprCore :: a -> String 

instance PPr CoreProgram where 
    pprCore = concatMap (\x -> pprCore x ++ "\n")

instance PPr CoreBind where 
    pprCore (NonRec b e) = show b `sp` "=" `sp` pprCore e 
    pprCore (Rec es)     = concatMap showTup es 
        where showTup (v,e) = show v `sp` "=" `sp` pprCore e 

instance PPr CoreExpr where 
    pprCore (Lam b e)    | isTyVar b = pprCore e 
                         | otherwise = "\\" ++ show b `sp` "->" `sp` pprCore e 
    pprCore (App e arg)  = pprCore e `sp` pprCore arg 
    pprCore (Var v)      | not (isTyVar v) = show v  
    pprCore (Lit l)      = show l  
    pprCore (Let b e)    = "let" `sp` show b `sp` "in" `sp` pprCore e  
    pprCore _            = ""

instance PPr [CoreExpr] where 
    pprCore = concatMap pprCore 

par :: String -> String 
par s = "(" ++ s ++ ")"



