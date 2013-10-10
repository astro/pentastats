{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
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
import Data.Conduit.Lazy
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP as HTTP
import Control.Exception (catch, SomeException)
import Network.URI (parseURI)
import qualified Crypto.Hash.MD5 as MD5
import Data.Hex
import qualified Data.Geolocation.GeoIP as Geo
import Data.List

import Shared
import SourceIter
import UAFilter


dataPath :: FilePath
dataPath = "public/data/"

aggregateStats :: [(Key, Value)] -> ResourceT IO ()
aggregateStats kvs = 
    do fileSizes <- liftIO loadFileSizes
       let index = Map.empty
       geoDB <- liftIO $ Geo.openGeoDB Geo.mmap_cache "/usr/share/GeoIP/GeoLiteCity.dat"
       let geoLocate = Geo.geoLocateByIPAddress geoDB
       uaFilter <- liftIO loadFilters
       
       let comparePath (k, _) (k', _) =
               kPath k == kPath k'
           compareDate (k, _) (k', _) =
               kDate k == kDate k'
               
       (fileSizes', index') <-
           foldM 
           (\(!fileSizes', !index') path_kvs ->
                do let path = {-# SCC "path" #-} kPath $ fst $ head path_kvs
                   (fileSize, fileSizes'') <- liftIO $ getFileSize fileSizes' path
                   (downloads, geo, uas) <- 
                       foldM 
                       (\(!downloads, !geo, !uas) date_kvs ->
                           do let day = T.pack $ show $
                                        kDate $ fst $ head date_kvs
                                  hosts_uas' :: Map.HashMap (BC.ByteString, BC.ByteString) Int
                                  hosts_uas' =
                                      foldl' (\hosts_uas (k, v) ->
                                                  let host_ua = (kHost k, kUserAgent k)
                                                      size = vSize v
                                                  in Map.insertWith (+)
                                                     host_ua size hosts_uas
                                             ) Map.empty date_kvs
                                  -- | Limit to max. 1 file size by (host, ua) per day
                                  hosts_uas'' :: Map.HashMap (BC.ByteString, BC.ByteString) Double
                                  hosts_uas'' =
                                      Map.map ((/ (fromIntegral fileSize)) .
                                               fromIntegral .
                                               min fileSize) hosts_uas'
                                  dayDownloads =
                                      sum $
                                      snd `map` Map.toList hosts_uas''
                              (geo', uas') <- 
                                  liftIO $
                                  foldM 
                                  (\(!geo', !uas') ((host, ua), hostUaDownloads) ->
                                       do let unknownCountry
                                                  | ':' `BC.elem` host = "v6"
                                                  | otherwise = "*"
                                          country <- maybe unknownCountry (decodeUtf8 . Geo.geoCountryCode) <$>
                                                     geoLocate host
                                          mUa <- uaFilter ua
                                          ua' <- case mUa of
                                                   Just ua' -> return ua'
                                                   Nothing ->
                                                       do --putStrLn $ "Unknown user-agent: " ++ show ua
                                                          return "*"
                                          return (Map.insertWith (+) 
                                                  country hostUaDownloads
                                                  geo',
                                                  Map.insertWith (+)
                                                  ua' hostUaDownloads
                                                  uas')
                                  ) (Map.empty :: Map.HashMap T.Text Double,
                                     Map.empty :: Map.HashMap T.Text Double) $
                                  Map.toList hosts_uas''
                              liftIO $ putStrLn $
                                     "day " ++ show day ++
                                     " downloads: " ++ show dayDownloads ++
                                     " geo: " ++ show geo' ++
                                     " uas: " ++ show uas'
                              return (Map.insertWith (+) day dayDownloads downloads,
                                      Map.insertWith (Map.unionWith (+)) day geo' geo,
                                      Map.insertWith (Map.unionWith (+)) day uas' uas)
                       ) (Map.empty, Map.empty, Map.empty) $
                       groupBy compareDate path_kvs

                   let totalDownloads =
                           sum $ Map.elems downloads
                       jsonName = hex $ MD5.hash path
                       jsonPath = dataPath ++ BC.unpack jsonName ++ ".json"
                   liftIO $ 
                     do putStrLn $ BC.unpack path
                        LBC.writeFile jsonPath $ JSON.encode $ JSON.object [
                                "downloads" .= Map.toList downloads,
                                "geo" .= Map.toList geo,
                                "user_agents" .= Map.toList uas
                               ]
                   let index'' =
                           seq totalDownloads $
                           Map.insert path
                                  (JSON.object [ 
                                            "json" .= jsonName,
                                            "downloads" .= totalDownloads
                                           ])
                           index'
                   return (fileSizes'', index'')
           ) (fileSizes, index) $
           groupBy comparePath kvs
                      
       liftIO $ saveFileSizes fileSizes'
       liftIO $ saveIndex index'
    
saveIndex :: JSON.ToJSON json => 
             json -> IO ()
saveIndex = 
    LBC.writeFile (dataPath ++ "index.json") . JSON.encode

fetchFileSize :: BC.ByteString -> IO (Maybe Int)
fetchFileSize path
    | not (':' `BC.elem` path) =
        return Nothing
    | otherwise =
        do putStrLn $ "HEAD " ++ BC.unpack path
           getSize <$> catch (HTTP.simpleHTTP headRequest)
                       (\(_::SomeException) -> return $ Left undefined)
    where uri = fromMaybe undefined $
                parseURI $ 
                BC.unpack path
          headRequest :: HTTP.Request BC.ByteString
          headRequest = HTTP.mkRequest HTTP.HEAD uri
          getSize (Right rsp) = read <$>
                                HTTP.findHeader HTTP.HdrContentLength rsp
          getSize _ = Nothing

sizesFile :: FilePath
sizesFile = "sizes.json"

type FileSizes = Map.HashMap BC.ByteString Int

loadFileSizes :: IO FileSizes
loadFileSizes = catch loadSizes (const $ return Map.empty :: SomeException -> IO FileSizes) >>=
                (\a ->
                     do print a
                        return a
                )
    where loadSizes :: IO FileSizes
          loadSizes = do Just json <- JSON.decode <$> LBC.readFile sizesFile
                         return json

saveFileSizes :: FileSizes -> IO ()
saveFileSizes =
    LBC.writeFile sizesFile . JSON.encode
        
getFileSize :: FileSizes -> BC.ByteString -> IO (Int, FileSizes)
getFileSize fileSizes path =
    do case path `Map.lookup` fileSizes of
         Just size -> return (size, fileSizes)
         Nothing ->
             do mSize <- fetchFileSize path
                case mSize of
                  Just size ->
                      do let fileSizes' =
                                    Map.insert path size fileSizes
                         return (size, fileSizes')
                  Nothing ->
                      return (fallbackSize, fileSizes)
                      
    where fallbackSize = 100 * 1024 * 1024  -- 100 MB


main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       kvs <- lazyConsume $
              sourceIter iter $=
              CL.mapMaybeM 
              (\(k, v) -> 
                  case ({-# SCC "convertK" #-} safeConvert k, 
                        {-# SCC "convertV" #-} safeConvert v) of
                    (Right k', Right v') ->
                        return $ Just (k', v')
                    (Left e, _) ->
                        do liftIO $ putStrLn $ "Cannot convert key: " ++ show e
                           return Nothing
                    (_, Left e) ->
                        do liftIO $ putStrLn $ "Cannot convert value: " ++ show e
                           return Nothing
              )
       aggregateStats kvs
