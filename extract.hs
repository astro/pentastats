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
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Text.Printf
import Data.List (sort)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Network.HTTP as HTTP
import Data.IORef
import Control.Exception (catch, SomeException)
import Network.URI (parseURI)

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
    do refFileSizes <- liftIO loadFileSizes
       
       let aggregate Nothing key value =
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
               do fileSize <- liftIO $ getFileSize refFileSizes path
                  let days' = JSON.object $
                          map (\(day, hosts) ->
                               let downloads :: Double
                                   downloads =
                                       sum $
                                       map ((/ (fromIntegral fileSize)) .
                                            fromIntegral .
                                            min fileSize .
                                            snd) $
                                       Map.toList hosts
                               in T.pack (show day) .= downloads
                              ) $
                          Map.toList days
                  liftIO $ 
                         do putStrLn $ BC.unpack path
                            putStrLn $ LBC.unpack $ JSON.encode days'
                            
       CL.mapMaybe (\(key, value) ->
                        case (safeConvert key, safeConvert value) of
                          (Right key', Right value') -> Just (key', value')
                          _ -> Nothing
                   ) =$ groupByPaths aggregate finalizeByPath Nothing Nothing
         
       liftIO $ saveFileSizes refFileSizes
    

fallbackSize = 100 * 1024 * 1024  -- 100 MB

fetchFileSize :: BC.ByteString -> IO Int
fetchFileSize path
    = do putStrLn $ "HEAD " ++ BC.unpack path
         getSize `liftM` HTTP.simpleHTTP headRequest
    where uri = fromMaybe undefined $
                parseURI uri'
          uri' | ':' `BC.elem` path = path'
               | otherwise = "http://ftp.c3d2.de" ++ path'
          path' = BC.unpack path
          headRequest :: HTTP.Request BC.ByteString
          headRequest = HTTP.mkRequest HTTP.HEAD uri
          getSize (Right rsp) = read $
                                fromMaybe (show fallbackSize) $
                                HTTP.findHeader HTTP.HdrContentLength rsp

sizesFile = "sizes.json"

type FileSizes = Map.HashMap BC.ByteString Int

loadFileSizes :: IO (IORef FileSizes)
loadFileSizes = catch loadSizes (const $ return Map.empty :: SomeException -> IO FileSizes) >>=
                (\a ->
                     do print a
                        return a
                ) >>=
                newIORef
    where loadSizes :: IO FileSizes
          loadSizes = do Just json <- JSON.decode <$> LBC.readFile sizesFile
                         return json

saveFileSizes :: IORef FileSizes -> IO ()
saveFileSizes refFileSizes =
    readIORef refFileSizes >>= 
    LBC.writeFile sizesFile . JSON.encode
        
getFileSize :: IORef FileSizes -> BC.ByteString -> IO Int
getFileSize refFileSizes path =
    do fileSizes <- readIORef refFileSizes
       case path `Map.lookup` fileSizes of
         Just size -> return size
         Nothing ->
             do size <- fetchFileSize path
                writeIORef refFileSizes $
                           Map.insert path size fileSizes
                return size

main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       sourceIter iter $$ aggregateStats
