{-# LANGUAGE ScopedTypeVariables #-}
module FileSizes (loadFileSizes, saveFileSizes, getFileSize) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Network.HTTP as HTTP
import Control.Exception (catch, SomeException)
import Network.URI (parseURI)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as Map
import Data.IORef
import System.Timeout (timeout)


fetchFileSize :: BC.ByteString -> IO (Maybe Integer)
fetchFileSize path
    | not (':' `BC.elem` path) =
        return Nothing
    | otherwise =
        do putStrLn $ "HEAD " ++ BC.unpack path
           join <$> timeout (httpTimeout * 1000000)
             (getSize <$> catch (HTTP.simpleHTTP headRequest)
              (\(_::SomeException) -> return $ Left undefined)
             )
    where uri = fromMaybe undefined $
                parseURI $ 
                BC.unpack path
          headRequest :: HTTP.Request BC.ByteString
          headRequest = HTTP.mkRequest HTTP.HEAD uri
          getSize (Right rsp) = read <$>
                                HTTP.findHeader HTTP.HdrContentLength rsp
          getSize _ = Nothing
          httpTimeout = 3

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
                         
