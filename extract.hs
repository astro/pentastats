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
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP as HTTP
import Data.IORef
import Control.Exception (catch, SomeException)
import Network.URI (parseURI)
import qualified Crypto.Hash.MD5 as MD5
import Data.Hex
import qualified Data.Geolocation.GeoIP as Geo
import Data.List

import Shared
import SourceIter
import UAFilter


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

dataPath :: FilePath
dataPath = "public/data/"

findPeak :: [(Date, Double)] -> Maybe Date
findPeak [] = Nothing
findPeak dayDownloads = 
    let ds = do d <- init $ tails dayDownloads
                let ((date, _):_) = d
                    downloads = sum $ map snd $ take 2 d
                return (date, downloads)
        peakDate = fst $
                   maximumBy (\(_, downloads1) (_, downloads2) ->
                                  downloads1 `compare` downloads2
                             ) ds
    in seq peakDate $
       Just peakDate

aggregateStats :: Sink (BC.ByteString, BC.ByteString) (ResourceT IO) ()
aggregateStats = 
    do refFileSizes <- liftIO loadFileSizes
       refIndex <- liftIO $ newIORef Map.empty
       geoDB <- liftIO $ Geo.openGeoDB Geo.mmap_cache "/usr/share/GeoIP/GeoLiteCity.dat"
       let geoLocate = Geo.geoLocateByIPAddress geoDB
       uaFilter <- liftIO loadFilters
       
       let aggregate Nothing key value =
               aggregate (Just Map.empty) key value
           aggregate (Just days) key value =
               return $
               let day = kDate key
                   host_ua = (kHost key, kUserAgent key)
                   size = vSize value
                   days' = case day `Map.lookup` days of
                             Nothing ->
                                 Map.insert day 
                                        (Map.singleton host_ua size) 
                                        days
                             Just hosts_uas ->
                                 Map.insert day
                                        (Map.insertWith (+) host_ua size hosts_uas)
                                        days
               in Just days'
                 
           finalizeByPath days path =
               do fileSize <- liftIO $ getFileSize refFileSizes path
                  (days', dayDownloads) <-
                      fmap unzip $
                      forM (Map.toList days) $ \(day,hosts_uas) ->
                      do -- | Relative to fileSize:
                              let hostUaDownloads = 
                                      Map.map ((/ (fromIntegral fileSize)) .
                                               fromIntegral .
                                               min fileSize) hosts_uas
                                  day' = T.pack $ show day
                                  downloads :: Double
                                  downloads = sum $
                                              snd `map` Map.toList hostUaDownloads
                              geo <- 
                                  liftIO $
                                  foldM (\(!geo) ((host, _ua), hostDownloads) ->
                                             do let unknown
                                                        | ':' `BC.elem` host = "v6"
                                                        | otherwise = "*"
                                                country <- maybe unknown (decodeUtf8 . Geo.geoCountryCode) <$>
                                                           geoLocate host
                                                return $
                                                       Map.insertWith (+) 
                                                       country hostDownloads
                                                       geo
                                         ) (Map.empty :: Map.HashMap T.Text Double) $
                                  Map.toList hostUaDownloads
                              userAgents <-
                                  liftIO $
                                  foldM (\(!uas) ((_host, ua), uaDownloads) ->
                                             do mUa <- uaFilter ua
                                                ua' <- case mUa of
                                                         Just ua' -> return ua'
                                                         Nothing ->
                                                             do --putStrLn $ "Unknown user-agent: " ++ show ua
                                                                return "*"
                                                return $
                                                       Map.insertWith (+)
                                                       ua' uaDownloads
                                                       uas
                                        ) (Map.empty :: Map.HashMap T.Text Double) $
                                  Map.toList hostUaDownloads
                              return ((day', downloads, geo, userAgents),
                                      (day, downloads))
                  let mPeakDate = findPeak $ sort dayDownloads
                      totalDownloads = sum daysDs
                      daysDays = map (\(day, _, _, _) -> day) days'
                      daysDs = map (\(_, d, _, _) -> d) days'
                      daysDownloads = JSON.object $
                                      zipWith (.=) daysDays daysDs
                      daysGs = map (\(_, _, g, _) -> g) days'
                      daysGeo = JSON.object $ 
                                zipWith (.=) daysDays daysGs
                      daysUas = map (\(_, _, _, ua) -> ua) days'
                      daysUserAgents = JSON.object $ 
                                       zipWith (.=) daysDays daysUas
                      
                  liftIO $ 
                         do putStrLn $ BC.unpack path
                            let jsonName = hex $ MD5.hash path
                                jsonPath = dataPath ++ BC.unpack jsonName ++ ".json"
                            LBC.writeFile jsonPath $ JSON.encode $ JSON.object [
                                    "downloads" .= daysDownloads,
                                    "geo" .= daysGeo,
                                    "user_agents" .= daysUserAgents
                                   ]
                            seq mPeakDate $ seq totalDownloads $
                                modifyIORef refIndex $
                                Map.insert path $
                                JSON.object 
                                        [ "json" .= jsonName,
                                          "peak" .= fmap show mPeakDate,
                                          "downloads" .= totalDownloads
                                        ]
                            
       CL.mapMaybe (\(key, value) ->
                        case (safeConvert key, safeConvert value) of
                          (Right key', Right value') -> Just (key', value')
                          _ -> Nothing
                   ) =$ groupByPaths aggregate finalizeByPath Nothing Nothing
         
       liftIO $ saveFileSizes refFileSizes
       liftIO $ saveIndex refIndex
    
saveIndex :: JSON.ToJSON json => 
             IORef json -> IO ()
saveIndex refIndex = 
    readIORef refIndex >>=
    LBC.writeFile (dataPath ++ "index.json") . JSON.encode

fetchFileSize :: BC.ByteString -> IO (Maybe Int)
fetchFileSize path
    | not (':' `BC.elem` path) =
        return Nothing
    | otherwise =
        do putStrLn $ "HEAD " ++ BC.unpack path
           getSize `liftM` HTTP.simpleHTTP headRequest
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
             do mSize <- fetchFileSize path
                case mSize of
                  Just size ->
                      do writeIORef refFileSizes $
                                    Map.insert path size fileSizes
                         return size
                  Nothing ->
                      return fallbackSize
                      
    where fallbackSize = 100 * 1024 * 1024  -- 100 MB


main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       sourceIter iter $$ aggregateStats
