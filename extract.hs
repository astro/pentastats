{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import Data.Default (def)
import qualified Database.LevelDB as DB
import Control.Monad.Trans.Resource
import Control.Monad.Trans
import Data.Conduit
import Data.Convertible
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.List as CL

import Shared
import SourceIter



data Grouped = PathStart BC.ByteString
             | DateStart Date
             | HostStart BC.ByteString
             | Size Int
             | HostEnd BC.ByteString
             | DateEnd Date
             | PathEnd BC.ByteString
               deriving (Show, Eq)

trackStates :: Monad m =>
               Conduit (BC.ByteString, BC.ByteString) m Grouped
trackStates = trackStates' Nothing Nothing Nothing
    
trackStates' :: Monad m =>
                Maybe BC.ByteString ->
                Maybe Date ->
                Maybe BC.ByteString ->
                Conduit (BC.ByteString, BC.ByteString) m Grouped
trackStates' mPath mDate mHost =
    do mKeyValue <- await
       let mKey = safeConvert <$> fst <$> mKeyValue >>= \lr ->
                  case lr of
                    Right key -> Just key
                    Left _ -> Nothing
       case mHost of
         Just host | mHost /= (kHost <$> mKey) -> yield $ HostEnd host
         _ -> return ()
       case mDate of
         Just date | mDate /= (kDate <$> mKey) -> yield $ DateEnd date
         _ -> return ()
       case mPath of
         Just path | mPath /= (kPath <$> mKey) -> yield $ PathEnd path
         _ -> return ()

       case kPath <$> mKey of
         Just path | mPath /= Just path -> yield $ PathStart path
         _ -> return ()
       case kDate <$> mKey of
         Just date | mDate /= Just date -> yield $ DateStart date
         _ -> return ()
       case kHost <$> mKey of
         Just host | mHost /= Just host -> yield $ HostStart host
         _ -> return ()

       case safeConvert <$> snd <$> mKeyValue of
         Just (Right (Value size _)) -> yield $ Size size
         _ -> return ()
         
       trackStates' (kPath <$> mKey)
                    (kDate <$> mKey)
                    (kHost <$> mKey)

stats :: Sink Grouped (ResourceT IO) ()
stats = CL.foldM (\(!days, !size) g ->
                      case g of
                        PathStart _ ->
                            return (0, 0)
                        Size size' ->
                            return (days, size + size')
                        DateStart _ ->
                            return (days + 1, size)
                        PathEnd path ->
                            do liftIO $ putStrLn $
                                      BC.unpack path ++ " " ++ 
                                      show size ++ " bytes in " ++
                                      show days ++ " days"
                               return (0, 0)
                        _ ->
                            return (days, size)
                 ) (0 :: Int, 0 :: Int) >>
        return ()

main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       sourceIter iter $$ trackStates =$ stats
