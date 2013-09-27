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
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Text.Printf
import Data.List (sort)

import Shared
import SourceIter


groupByPaths :: (Maybe st -> Key -> Value -> ResourceT IO (Maybe st)) ->
                (st -> BC.ByteString -> ResourceT IO ()) ->
                Maybe BC.ByteString -> 
                Maybe st -> 
                Sink (Key, Value) (ResourceT IO) ()
groupByPaths aggregate finalizeByPath mPath mState =
    do mKeyValue <- await
       let mKey = fst <$> mKeyValue
       mState' <- case (mPath, mState) of
                    (Just path, Just state)
                        | Just path /= (kPath <$> mKey) ->
                            do lift $ finalizeByPath state path
                               return Nothing
                    (_, _) ->
                        return mState
       case mKeyValue of
         Just (key, value) ->
             do mState'' <- lift $ aggregate mState' key value
                groupByPaths aggregate finalizeByPath 
                      (Just $ kPath key) mState''
         Nothing ->
             return ()

aggregateStats :: Sink (BC.ByteString, BC.ByteString) (ResourceT IO) ()
aggregateStats =
    CL.mapMaybe (\(key, value) ->
                     case (safeConvert key, safeConvert value) of
                       (Right key', Right value') -> Just (key', value')
                       _ -> Nothing
                ) =$
    groupByPaths aggregate finalizeByPath Nothing Nothing
    
    where aggregate Nothing key value =
              aggregate (Just Map.empty) key value
          aggregate (Just days) key value =
              return $
              let day = kDate key
                  host = kHost key
                  size = vSize value
                  days' = case day `Map.lookup` days of
                            Nothing ->
                                Map.insert day 
                                (Map.singleton host size) 
                                days
                            Just hosts ->
                                Map.insert day
                                (Map.insertWith (+) host size hosts)
                                days
              in Just days'
                 
          finalizeByPath days path =
              do let fileSize = 100 * 1024 * 1024 :: Int
                     days' = sort $
                             map (\(day, hosts) ->
                                      let downloads :: Double
                                          downloads =
                                              sum $
                                              map ((/ (fromIntegral fileSize)) .
                                                   fromIntegral .
                                                   min fileSize .
                                                   snd) $
                                              Map.toList hosts
                                      in (day, downloads)
                                 ) $
                             Map.toList days
                 liftIO $ 
                        do putStrLn $ BC.unpack path
                           forM_ days' $ \(day, downloads) ->
                               printf "\t%s\t%.2f\n" (show day) downloads

main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       sourceIter iter $$ aggregateStats
