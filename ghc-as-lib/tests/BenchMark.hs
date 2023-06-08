

import Criterion


import Test (analyseItems,TestItem, decodeJson)
import Criterion.Main (defaultMain, defaultConfig, defaultMainWith)
import Criterion.Types (Config(resamples))
import Criterion.Monad

path = "/home/sauron/ae_test/ghc-as-lib/ghc-as-lib/ghc-as-lib/studentfiles/"

complete15,complete23, ontrack15, ontrack23 :: IO [TestItem] 
complete15 = decodeJson (path ++ "complete.json")
complete23 = decodeJson (path ++ "completenew.json")
ontrack15  = decodeJson (path ++ "ontrack.json")
ontrack23  = decodeJson (path ++ "ontracknew.json")

{- f :: FilePath -> String -> IO () 
f sf mfs = analyse_ sf (mpath ++ mfs) >> return ()  -}

main = defaultMainWith (defaultConfig { resamples = 10 })  
          [bgroup "diagnose" 
            [
            -- bench "diag_complet15" $ nfIO $ complete15 >>= analyseItems
            --,bench "diag_complete23" $ nfIO $ complete23 >>= analyseItems
            bench "diag_ontrack15" $ nfIO $ ontrack15 >>= analyseItems
            ,bench "diag_ontrack23" $ nfIO $ ontrack23 >>= analyseItems
            ]
            
          ]
           -- [ bgroup "diag" [
           -- 
           -- ,bench "diag_complete23" $ nfIO (analyseItems file )    
           -- ,bench "diag_ontrack15"  $ nfIO (analyseItems file)
           -- ,bench "diag_ontrack23"  $ nfIO (analyseItems file)
           -- ] ]
