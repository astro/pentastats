{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Attoparsec.Lazy hiding (take, takeTill, takeWhile)
import Data.Attoparsec.Char8 (peekChar, anyChar, char, char8, takeWhile, takeTill, isDigit, isAlpha_ascii)
import Prelude hiding (takeWhile)
import Control.Monad (liftM, foldM, when)
import Data.Time.Clock
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import qualified Data.HashMap.Strict as Map
import System.IO
import Network.URI (parseURI)
import GHC.Real (Ratio((:%)))
import Debug.Trace
import Data.Char (ord)
import qualified Database.LevelDB as DB
import Data.Default (def)
import Control.Monad.Trans.Resource
import Control.Monad.Trans

import Shared


data Request = Request {
     reqMethod :: !BC.ByteString,
     reqCode :: !Int, 
     reqDate :: !Date, 
     reqTime :: !DayTime,
     reqPath :: !BC.ByteString, 
     reqHost :: !BC.ByteString, 
     reqSize :: !(Maybe Int),
     reqUserAgent :: !BC.ByteString
  } deriving (Show)

parseLine :: LBC.ByteString -> Result Request
parseLine = {-# SCC "parse" #-} parse line
    where line = do host <- {-# SCC "wordHost" #-} word
                    space
                    ident <- {-# SCC "wordIdent" #-} word
                    space
                    user <- {-# SCC "wordUser" #-} word
                    space
                    char '['
                    date <- {-# SCC "date" #-} date
                    char ':'
                    time <- {-# SCC "time" #-} time
                    -- ignore TZ info
                    takeTill (== ']')
                    string "] \""
                    method <- {-# SCC "wordMethod" #-} word
                    space
                    path <- {-# SCC "wordPath" #-} word
                    space
                    ver <- {-# SCC "wordVer" #-} word
                    space
                    code <- {-# SCC "wordCode" #-} num
                    space
                    mSize <- {-# SCC "wordSize" #-} 
                             (pure Nothing <* char '-') <|>
                             (Just <$> num)
                    space
                    char '"'
                    referrer <- {-# SCC "referrer" #-} takeTill (== '"')
                    char '"'
                    space
                    char '"'
                    userAgent <- {-# SCC "userAgent" #-} takeTill (== '"')
                    char '"'
                    eol
                    return $ {-# SCC "Request" #-} Request method code date time path host mSize userAgent
          space = char ' '
          word = takeTill (== ' ')
          num = let loop !n =
                      do mC <- peekChar
                         case mC of
                           Just c
                             | c >= '0' && c <= '9' ->
                               anyChar *> loop (n * 10 + ord c - ord '0')
                           _ ->
                             return n
                in loop 0
          date = do day <- num
                    char '/'
                    mon <- month
                    char '/'
                    year <- num
                    return $ Date year mon day
          month = fromMaybe (error "Invalid month") <$>
                  flip Map.lookup months <$> 
                  takeWhile isAlpha_ascii
            where
              months = Map.fromList $ zip months' [1..]
              months' = map BC.pack months''
              months'' = ["Jan", "Feb", "Mar",
                          "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep",
                          "Oct", "Nov", "Dec"]
          time = DayTime <$>
                 num <*
                 char ':' <*>
                 num <*
                 char ':' <*>
                 num
          eol = char '\n'
                         
parseFile :: LBC.ByteString -> [Request]
parseFile s 
  | LBC.null s = []
  | otherwise =
    case parseLine s of
      Done rest req ->
          req : parseFile rest
      Fail rest _ errMsg ->
          trace errMsg $
          let rest' = LBC.dropWhile (/= '\n') rest
          in parseFile $ LBC.tail rest'


reqKey :: Request -> BC.ByteString
reqKey req = BC.concat
             [reqPath req,
              BC.singleton '\n',
              BC.pack $ show $ reqDate req,
              BC.singleton '\n',
              BC.pack $ show $ reqTime req,
              BC.singleton '\n',
              reqHost req,
              BC.singleton '\n',
              reqUserAgent req
             ]

-- TODO: normalize path

groupByDateTime :: [Request] -> [[Request]]
groupByDateTime [] = []
groupByDateTime (req : reqs) =
    let date = reqDate req
        time = reqTime req
        (reqs', reqs'') = break
                          (\req' ->
                               time /= reqTime req' ||
                               date /= reqDate req'
                          ) reqs
    in (req : reqs') : groupByDateTime reqs''


-- Returns amount of keys written
writeReqs :: DB.DB -> [Request] -> ResourceT IO Int
writeReqs db reqs =
    do let keysValues =
               Map.toList $
               foldl' (\map req ->
                           case reqSize req of
                             Just size 
                                 | size > 0 ->
                                     Map.insertWith (+) 
                                     (reqKey req) size map
                             _ ->
                                 map
                      ) Map.empty reqs
           ops = map (\(key, value) ->
                          DB.Put key $ BC.pack $ show value
                     ) keysValues
       DB.write db def ops
       return $ length ops


main :: IO ()
main = 
    runResourceT $
    do db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       (lines, records) <- parseFile <$> lift LBC.getContents >>=
                foldM 
                (\(!lines, !records) reqs ->
                     do let lines' = length reqs
                        records' <- writeReqs db reqs
                        return (lines + lines', records + records')
                ) (0 :: Int, 0 :: Int) .
                groupByDateTime .
                filter (\req ->
                            reqMethod req == "GET" &&
                            reqCode req >= 200 && 
                            reqCode req < 300 
                       )
       lift $ putStrLn $ "Processed " ++ show lines ++ " lines into " ++ show records ++ " records"
