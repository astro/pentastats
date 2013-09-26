-- | https://gist.github.com/ssadler/f1d7f799eef47b025c9c
module SourceIter where
 
import Control.Monad (when)
import Control.Applicative    ((<$>), (<*>))
 
import Data.Conduit
 
import Data.ByteString        (ByteString)
 
import Database.LevelDB
 
import Control.Monad.Trans
import System.IO
  
sourceIter :: MonadResource m =>
              Iterator -> ConduitM i (ByteString, ByteString) m ()
sourceIter iter = do
    mkey <- iterKey iter
    mval <- iterValue iter
    let mpair  = (,) <$> mkey <*> mval
    case mpair of
        Nothing -> return ()
        Just pair -> do
            yield pair
            iterNext iter
            valid <- iterValid iter
            case valid of
              True ->
                  sourceIter iter
              False ->
                  liftIO $ hPutStrLn stderr "done"
