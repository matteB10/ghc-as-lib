module Warning where 

import GHC ( Severity, SrcSpan ) 
import GHC.Driver.Session (WarnReason(..))
import GHC.Types.Error (SDoc)
import GHC.Driver.Ppr (showSDoc)
import GHC.Utils.Logger (LogAction)
import Data.IORef (IORef, modifyIORef)


data Warning = GhcWarn {reason :: WarnReason,
                        sev :: Severity,
                        span :: SrcSpan,
                        doc :: SDoc}
    deriving Show 

instance Eq WarnReason where
  (Reason flag) == (Reason flag') = flag == flag'
  (ErrReason f) == (ErrReason f') = f == f'
  NoReason == NoReason = True
  _ == _               = False

instance Eq Warning where 
  GhcWarn r s sp doc == GhcWarn r' s' sp' doc' = r == r' && sp == sp' -- for checking the same program, comparing warnings from model/student requires other handling


writeWarnings :: IORef [Warning] -> LogAction -> LogAction
writeWarnings ref action dflags reason sev span doc = do
  modifyIORef ref (GhcWarn reason sev span doc:)
  noAction dflags reason sev span doc -- don't log any actions 

noAction :: LogAction
noAction _ _ _ _ _ = return () 