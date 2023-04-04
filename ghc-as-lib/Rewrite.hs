module Rewrite where 
import GHC
import qualified Language.Haskell.TH as TH 
import Language.Haskell.TH.Syntax ( Exp, Q (unQ), liftData, lift )  
import Language.Haskell.TH (runQ)


mkTH :: HsModule -> Q Exp    
mkTH ps = do 
      ps' <- liftData ps 
      return ps' 
 

rewrite :: Q Exp -> Q Exp 
rewrite e = do 
    e' <- e 
    case e' of 
        ex ->  do 
          v' <- TH.newName "---    FRESH   -----"
          return $ TH.VarE v' 