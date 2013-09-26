{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Attoparsec.Lazy hiding (take, takeTill, takeWhile)
import Data.Attoparsec.Char8 (peekChar, anyChar, char, char8, takeWhile, takeTill, isDigit, isAlpha_ascii)
import Prelude hiding (takeWhile)
import Control.Monad
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
import Data.Convertible
import System.Time (getClockTime)
import qualified Data.Set as Set

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


reqKey :: Request -> Key
reqKey req = Key
             (reqPath req)
             (reqDate req)
             (reqHost req)
             (reqUserAgent req)

group :: [Request] -> [[Request]]
group [] = []
group (req : reqs) =
    let k = reqKey req
        (reqs', reqs'') = break ((k /=) . reqKey) reqs
    in (req : reqs') : group reqs''


-- TODO: could do more
normalizePaths :: [Request] -> [Request]
normalizePaths = map $ \req ->
                 req { reqPath = rmDots $ BC.takeWhile (/= '?') $ reqPath req }
  where rmDots b
          | BC.length b >= 2 &&
            BC.take 2 b == "//" = rmDots $ BC.drop 1 b
          | BC.length b >= 4 &&
            BC.take 4 b == "/../" = rmDots $ BC.drop 3 b
          | BC.length b >= 3 &&
            BC.take 3 b == "/./" = rmDots $ BC.drop 2 b
          | otherwise = b

-- Returns amount of keys written
writeReqs :: DB.DB -> BC.ByteString -> [Request] -> ResourceT IO Int
writeReqs db token reqs =
    do let key = convert $ reqKey $ head reqs
       mOldValue <- (safeConvert <$>) <$> DB.get db def key
       let reqsSize = sum $ map (fromMaybe 0 . reqSize) reqs
           newValue = case mOldValue of
                         Just (Right value)
                           | vToken value == token ->
                             -- Was modified during this session, add:
                             value { vSize = vSize value + reqsSize }
                         _ ->
                           -- First time seen
                           Value { vSize = reqsSize,
                                   vToken = token }
       DB.put db def key $ convert $ newValue
       return 1


main :: IO ()
main = 
    runResourceT $
    do token <- BC.pack <$> show <$> liftIO getClockTime
       db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       (lines, records, bytes) <- parseFile <$> lift LBC.getContents >>=
                foldM 
                (\(!lines, !records, !bytes) reqs ->
                     do let lines' = length reqs
                            bytes' = sum $ map (fromMaybe 0 . reqSize) reqs
                        --liftIO $ putStrLn $ "reqs: " ++ show reqs
                        records' <- writeReqs db token reqs
                        return (lines + lines', records + records', bytes + bytes')
                ) (0 :: Int, 0 :: Int, 0 :: Int) .
                group .
                normalizePaths .
                filter (\req ->
                            reqMethod req == "GET" &&
                            reqCode req >= 200 && 
                            reqCode req < 300 
                       )
       lift $ putStrLn $ "Processed " ++ show lines ++ 
         " lines into " ++ show records ++ 
         " records (" ++ show bytes ++ " bytes)"
