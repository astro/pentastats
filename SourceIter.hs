-- | https://gist.github.com/ssadler/f1d7f799eef47b025c9c
module SourceIter where
 
import Control.Applicative    ((<$>), (<*>))
 
import Data.Conduit
 
import Data.ByteString        (ByteString)
 
import Database.LevelDB
 
 
sourceIter :: MonadResource m =>
              Iterator -> ConduitM i (ByteString, ByteString) m ()
sourceIter iter = do
    mkey <- iterKey iter
    mval <- iterValue iter
    let mpair  = (,) <$> mkey <*> mval
    case mpair of
        Nothing -> return ()
        Just pair -> do
            _ <- iterNext iter
            yield pair
            sourceIter iter
 