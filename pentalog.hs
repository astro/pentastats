{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Attoparsec.Lazy hiding (take, takeTill, takeWhile)
import Data.Attoparsec.Char8 (peekChar, anyChar, char, char8, takeWhile, takeTill, isDigit, isAlpha_ascii)
import Prelude hiding (takeWhile)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import qualified Data.HashMap.Strict as Map
import System.IO
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
import System.Environment

import Shared


data Request = Request {
     reqMethod :: !BC.ByteString,
     reqCode :: !Int, 
     reqDate :: !Date, 
     reqTime :: !DayTime,
     reqPath :: !BC.ByteString, 
     reqHost :: !BC.ByteString, 
     reqSize :: !(Maybe Integer),
     reqReferer :: !BC.ByteString,
     reqUserAgent :: !BC.ByteString
  } deriving (Show)

parseLine :: BC.ByteString -> LBC.ByteString -> Result Request
parseLine defaultHostname = {-# SCC "parse" #-} 
  parse $
  try (do vhost <- word
          space
          prependHost vhost <$> line
      ) <|>
  (prependHost defaultHostname <$>
   line)
    where prependHost host req
              | BC.take 3 (snd $ 
                           "://" `BC.breakSubstring` reqPath req
                          ) == "://" =
                  req
              | BC.drop (BC.length host - 3) host == ":80" =
                  prependHost (BC.take (BC.length host - 3) host) req
              | otherwise =
                  req { reqPath = "http://" `BC.append` 
                                  host `BC.append` reqPath req
                  }
          line = do host <- {-# SCC "wordHost" #-} word
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
                             (Just <$> fromIntegral <$> num)
                    space
                    char '"'
                    referer <- {-# SCC "referer" #-} takeTill (== '"')
                    -- TODO: was defaultHostname?
                    char '"'
                    space
                    char '"'
                    userAgent <- {-# SCC "userAgent" #-} takeTill (== '"')
                    char '"'
                    eol
                    return $ {-# SCC "Request" #-} Request method code date time path host mSize referer userAgent
          space = char ' '
          word = takeTill (== ' ')
          num = let loop !n =
                      do mC <- peekChar
                         case mC of
                           Just c
                             | c >= '0' && c <= '9' ->
                               let n' = n * 10 + fromIntegral (ord c - ord '0')
                               in anyChar *> loop n'
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
                         
parseFile :: BC.ByteString -> LBC.ByteString -> [Request]
parseFile defaultHostname s 
  | LBC.null s = []
  | otherwise =
    case parseLine defaultHostname s of
      Done rest req ->
          req : parseFile defaultHostname rest
      Fail rest _ errMsg ->
          trace (errMsg ++ " in " ++ show (LBC.take 32 rest)) $
          let rest' = LBC.dropWhile (/= '\n') rest
          in parseFile defaultHostname $ LBC.tail rest'


reqKey :: Request -> Key
reqKey req = Key
             (reqPath req)
             (reqDate req)
             (reqHost req)
             (reqUserAgent req)
             (reqReferer req)

group :: [Request] -> [[Request]]
group [] = []
group (req : reqs) =
    let k = reqKey req
        (reqs', reqs'') = break ((k /=) . reqKey) reqs
    in (req : reqs') : group reqs''


-- TODO: could do more
normalizePaths :: [Request] -> [Request]
normalizePaths = map normalizePath'
  where normalizePath' req =
            req { reqPath = BC.takeWhile (/= '?') $ rmDots $ reqPath req }
        rmDots b = case "://" `BC.breakSubstring` b of
                     (scheme, b')
                         | BC.take 3 b' == "://" ->
                             let b'' = BC.drop 3 b'
                                 (host, path) = "/" `BC.breakSubstring` b''
                                 path' = rmDots' path
                             in BC.concat 
                                    [scheme, "://",
                                     host, rmDupSlashes path']
                     _ ->
                         rmDots' b
        rmDots' b
          | BC.length b >= 2 &&
            BC.take 2 b == "//" = rmDots' $ BC.drop 1 b
          | BC.length b >= 4 &&
            BC.take 4 b == "/../" = rmDots' $ BC.drop 3 b
          | BC.length b >= 3 &&
            BC.take 3 b == "/./" = rmDots' $ BC.drop 2 b
          | otherwise = b
        rmDupSlashes b = case "//" `BC.breakSubstring` b of
                           (b', b'') | BC.length b'' >= 2 &&
                                       BC.take 2 b'' == "//" ->
                                         b' `BC.append` rmDupSlashes (BC.tail b'')
                           (b', b'') | BC.null b'' ->
                                         b'

-- Returns amount of keys written
writeReqs :: DB.DB -> BC.ByteString -> [Request] -> ResourceT IO Integer
writeReqs db token reqs =
    do let key = convert $ reqKey $ head reqs
       mOldValue <- (safeConvert <$>) <$> DB.get db def key
       let reqsSize = sum $ map (fromMaybe 0 . reqSize) reqs
           newValue = case mOldValue of
                         Just (Right value)
                           | vToken value == token ->
                             -- Was modified during this session, add:
                             value { vCount = vCount value + 1,
                                     vSize = vSize value + reqsSize }
                         _ ->
                           -- First time seen
                           Value { vCount = 1,
                                   vSize = reqsSize,
                                   vToken = token }
       DB.put db def key $ convert $ newValue
       return 1


main :: IO ()
main = 
    do args <- getArgs
       let defaultHostname' =
               BC.pack $
               case args of
                 [defaultHostname] -> defaultHostname
                 [] -> "localhost"
       main' defaultHostname'

main' :: BC.ByteString -> IO ()
main' defaultHostname =
    runResourceT $
    do token <- BC.pack <$> show <$> liftIO getClockTime
       db <- DB.open "state" $ DB.defaultOptions { DB.createIfMissing = True }
       (lines, records, bytes) <- 
           parseFile defaultHostname <$> lift LBC.getContents >>=
           foldM 
           (\(!lines, !records, !bytes) reqs ->
                do let lines' = fromIntegral $ length reqs
                       bytes' = sum $ map (fromMaybe 0 . reqSize) reqs
                   --liftIO $ putStrLn $ "reqs: " ++ show reqs
                   records' <- writeReqs db token reqs
                   return (lines + lines', records + records', bytes + bytes')
           ) (0 :: Integer, 0 :: Integer, 0 :: Integer) .
           group .
           normalizePaths .
           filter (\req ->
                       reqMethod req == "GET" &&
                       reqCode req >= 200 && 
                       reqCode req < 400 
                  )
       lift $ putStrLn $ "Processed " ++ show lines ++ 
         " lines into " ++ show records ++ 
         " records (" ++ show bytes ++ " bytes)"
