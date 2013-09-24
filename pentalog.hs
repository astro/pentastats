{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances, OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Attoparsec.Lazy hiding (take, takeTill, takeWhile)
import Data.Attoparsec.Char8 (peekChar, anyChar, char, char8, takeWhile, takeTill, isDigit, isAlpha_ascii)
import Prelude hiding (takeWhile)
import Control.Monad (liftM, forM_, forM, when)
import Data.Time.Clock
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Network.URI (parseURI)
import GHC.Real (Ratio((:%)))
import Debug.Trace
import Data.Char (ord)


data DayTime = DayTime Int Int Int
             deriving (Show, Ord, Eq)

data Request = Request {
     reqMethod :: !BC.ByteString,
     reqCode :: !Int, 
     reqDate :: !Day, 
     reqTime :: !DayTime,
     reqPath :: !BC.ByteString, 
     reqHost :: !BC.ByteString, 
     reqSize :: !(Maybe Int)
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
                    return $ {-# SCC "Request" #-} Request method code date time path host mSize
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
                    year <- fromIntegral <$> num
                    return $ fromGregorian year mon day
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

-- filter GET, 200+206, normalize path

main :: IO ()
main = LBC.getContents >>=
       print . foldl' (\sum req -> 
                           sum + fromMaybe 0 (reqSize req)
                      ) 0 . parseFile
