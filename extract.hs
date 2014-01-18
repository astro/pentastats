{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
import Data.Monoid
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
import Control.Exception (catch, SomeException)
import Network.URI (parseURI)
import qualified Crypto.Hash.MD5 as MD5
import Data.Hex
import qualified Data.Geolocation.GeoIP as Geo
import Data.IORef
import Data.List (foldl')
import Data.Attoparsec.Number (Number(I, D))

import Shared
import SourceIter
import UAFilter
import Aggregate


dataPath :: FilePath
dataPath = "public/data/"


data DownloadsMetric = DownloadsByCount DownloadsCount
                     | DownloadsBySize Double
    deriving (Show, Eq)

instance Monoid DownloadsMetric where
  mempty = DownloadsByCount 0
  (DownloadsByCount c) `mappend` (DownloadsByCount c') = DownloadsByCount $ c + c'
  m `mappend` m' = let c = coerce m
                       c' = coerce m'
                       coerce (DownloadsByCount (DownloadsCount c)) = fromIntegral c
                       coerce (DownloadsBySize c) = c
                   in DownloadsBySize $ c + c'

instance JSON.ToJSON DownloadsMetric where
  toJSON m = JSON.Number $
             case m of
               DownloadsByCount (DownloadsCount c) -> I c
               DownloadsBySize c -> D c

toMetric :: Maybe Integer -> Value -> DownloadsMetric
toMetric mFileSize value
  | fileSize <= 0 =
    DownloadsByCount $ vCount value
  | otherwise =
    let DownloadsSize size = vSize value
    in DownloadsBySize $ fromIntegral size / fromIntegral fileSize
  where fileSize = fromMaybe 0 mFileSize

limitMetric :: DownloadsMetric -> DownloadsMetric
limitMetric (DownloadsByCount count) = DownloadsByCount $ min 1 count
limitMetric (DownloadsBySize count) = DownloadsBySize $ min 1 count

mapToObject :: JSON.ToJSON j => (k -> String) -> Map.HashMap k j -> JSON.Value
mapToObject show =
  JSON.object .
  map (\(k, v) ->
        T.pack (show k) .= v
      ) .
  Map.toList

aggregateStats :: ResourceT IO (Aggregate (Key, Value) (ResourceT IO) ())
aggregateStats = 
    do refFileSizes <- liftIO loadFileSizes
       geoDB <- liftIO $ Geo.openGeoDB Geo.mmap_cache "/usr/share/GeoIP/GeoLiteCity.dat"
       let geoLocate host = let unknownCountry
                                       | ':' `BC.elem` host = "v6"
                                       | otherwise = "*"
                            in maybe unknownCountry (decodeUtf8 . Geo.geoCountryCode) <$>
                               Geo.geoLocateByIPAddress geoDB host
       uaFilter <- liftIO loadFilters
       
       let aggregateByDay :: Aggregate 
                             (Key, Value)
                             (ResourceT IO) 
                             (BC.ByteString, Date, Map.HashMap (BC.ByteString, BC.ByteString) Value, Map.HashMap BC.ByteString Integer)
           aggregateByDay =
               foldAggregate 
               (\(_, _, !hosts_uas, !referers) (k, v) ->
                    do let path = kPath k
                           day = kDate k
                           host_ua = (kHost k, kUserAgent k)
                           referer = kReferer k
                           referers'
                             | referer == "" || referer == "-" =
                               referers
                             | otherwise =
                               Map.insertWith (+) referer 1 referers
                       return 
                           (path, day, 
                            Map.insertWith mappend host_ua v hosts_uas,
                            referers'
                           )
               ) (undefined, undefined, Map.empty, Map.empty) return
               
           aggregateByPath :: Aggregate 
                              (BC.ByteString, Date, Map.HashMap (BC.ByteString, BC.ByteString) Value, Map.HashMap BC.ByteString Integer)
                              (ResourceT IO) 
                              (BC.ByteString, BC.ByteString, DownloadsMetric)
           aggregateByPath =
               foldAggregate 
               (\(_, !downloads, !geo, !uas, !referers) (!path, !day, hosts_uas, day_referers) ->
                do mFileSize <- liftIO $ getFileSize refFileSizes path
                   let -- | Limit to max. 1 file size by (host, ua) per day
                       hosts_uas' :: Map.HashMap (BC.ByteString, BC.ByteString) DownloadsMetric
                       hosts_uas' =
                           Map.map (limitMetric . toMetric mFileSize) hosts_uas
                       dayMetric =
                           mconcat $
                           Map.elems hosts_uas'

                   hostCountries <- 
                       foldM (\(!hostCountries) host ->
                                  case host `Map.member` hostCountries of
                                    True ->
                                        return hostCountries
                                    False ->
                                        do country <- liftIO $ geoLocate host
                                           return $
                                                  Map.insert host country hostCountries
                             ) Map.empty $
                       map fst $
                       Map.keys hosts_uas'
                   uaNames <-
                       foldM (\(!uaNames) ua ->
                                  case ua `Map.member` uaNames of
                                    True ->
                                        return uaNames
                                    False ->
                                        do name <- fromMaybe "*" <$>
                                                   liftIO (uaFilter ua)
                                           -- when (name == "*") $
                                           --      liftIO $ putStrLn $
                                           --      "Unknown UA: " ++ show ua
                                           return $
                                                  Map.insert ua name uaNames
                             ) Map.empty $
                       map snd $
                       Map.keys hosts_uas
                       
                   let (geo', uas') =
                           foldl' 
                           (\(!geo', !uas') ((host, ua), hostUaDownloads) ->
                                let Just country = host `Map.lookup` hostCountries
                                    Just ua' = ua `Map.lookup` uaNames
                                in  (Map.insertWith mappend country hostUaDownloads geo',
                                     Map.insertWith mappend ua' hostUaDownloads uas')
                           ) (Map.empty :: Map.HashMap T.Text DownloadsMetric,
                              Map.empty :: Map.HashMap T.Text DownloadsMetric) $
                           Map.toList hosts_uas'
                   -- liftIO $ putStrLn $
                   --            "day " ++ show day ++
                   --            "\tdownloads: " ++ show dayDownloads ++
                   --            "\tgeo: " ++ show (Map.size geo') ++
                   --            "\tuas: " ++ show (Map.size uas')
                   return (path,
                           Map.insertWith mappend day dayMetric downloads,
                           Map.insertWith (Map.unionWith mappend) day geo' geo,
                           Map.insertWith (Map.unionWith mappend) day uas' uas,
                           Map.unionWith (+) referers day_referers
                          )
               ) (undefined, Map.empty, Map.empty, Map.empty, Map.empty) $
               \(path, downloads, geo, uas, referers) ->
                   do let totalDownloads =
                              mconcat $ Map.elems downloads
                          jsonName = hex $ MD5.hash path
                          jsonPath = dataPath ++ BC.unpack jsonName ++ ".json"
                      liftIO $ putStrLn $ BC.unpack path ++ " (" ++ show totalDownloads ++ ")"
                      liftIO $ LBC.writeFile jsonPath $ JSON.encode $ JSON.object [
                                        "downloads" .= mapToObject show downloads,
                                        "geo" .= mapToObject show geo,
                                        "user_agents" .= mapToObject show uas,
                                        "referers" .= mapToObject BC.unpack referers
                                       ]
                      return (path, jsonName, totalDownloads)
                      
           aggregateAll :: Aggregate (BC.ByteString, BC.ByteString, DownloadsMetric) (ResourceT IO) ()
           aggregateAll =
               foldAggregate 
               (\index (!path, !jsonName, !totalDownloads) ->
                    do let k = decodeUtf8 path 
                           v = JSON.object [ 
                                "json" .= jsonName,
                                "downloads" .= totalDownloads
                               ]
                       return $ (k .= v) : index
               ) [] $
               \index ->
                   do liftIO $ saveIndex $ JSON.object index
                      liftIO $ saveFileSizes refFileSizes
                      return ()
       
       let comparePath (k, _) (k', _) =
               kPath k == kPath k'
           compareDate (k, _) (k', _) =
               kDate k == kDate k'
               
       return $ groupAggregate comparePath
              (groupAggregate compareDate aggregateByDay aggregateByPath) aggregateAll
                      
saveIndex :: JSON.ToJSON json => 
             json -> IO ()
saveIndex = 
    LBC.writeFile (dataPath ++ "index.json") . JSON.encode

fetchFileSize :: BC.ByteString -> IO (Maybe Integer)
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

type FileSizes = Map.HashMap BC.ByteString (Maybe Integer)

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
        
getFileSize :: IORef FileSizes -> BC.ByteString -> IO (Maybe Integer)
getFileSize refFileSizes path =
    do fileSizes <- readIORef refFileSizes
       case path `Map.lookup` fileSizes of
         Just mSize -> 
           return mSize
         Nothing ->
           do mSize <- fetchFileSize path
              writeIORef refFileSizes $
                Map.insert path mSize fileSizes
              return mSize
                         


main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       iter <- DB.iterOpen db def
       DB.iterFirst iter
       aggregator <- aggregateStats
       sourceIter iter $$
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
              ) =$
              aggregateSink aggregator
