{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Attoparsec.Lazy hiding (take)
import Prelude hiding (takeWhile)
import Data.Char (isDigit, isAlpha)
import Control.Monad (liftM, forM_, forM, when)
import Data.Time.Clock
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System.Cmd (system)
import System.Directory (removeFile)
import Network.URI (parseURI)
import Text.Printf (printf)
import Data.ByteString.Internal (w2c, c2w)
import GHC.Real (Ratio((:%)))
import Data.Network.Address
import Debug.Trace


data Host = Host4 !IPv4
          | Host6 !IPv6
            deriving (Show, Eq, Ord)
data Request = Request !BC.ByteString !Int !Day !BC.ByteString !Host !(Maybe Integer)
             deriving (Show)

parseLine :: LBC.ByteString -> Result Request
parseLine = {-# SCC "parse" #-} parse line
    where line = do host <- {-# SCC "wordHost" #-} host
                    space
                    ident <- {-# SCC "wordIdent" #-} word
                    space
                    user <- {-# SCC "wordUser" #-} word
                    space
                    char '['
                    date <- {-# SCC "date" #-} date
                    takeWhile ((/= '"') . w2c)
                    char '"'
                    method <- {-# SCC "wordMethod" #-} word
                    space
                    path <- {-# SCC "wordPath" #-} word
                    space
                    ver <- {-# SCC "wordVer" #-} word
                    space
                    code <- {-# SCC "wordCode" #-} num'
                    space
                    mSize <- {-# SCC "wordSize" #-} 
                             (pure Nothing <* char '-') <|>
                             (Just <$> num)
                    space
                    char '"'
                    referrer <- {-# SCC "referrer" #-} takeWhile ((/= '"') . w2c)
                    char '"'
                    space
                    char '"'
                    userAgent <- {-# SCC "userAgent" #-} takeWhile ((/= '"') . w2c)
                    char '"'
                    eol
                    return $ {-# SCC "Request" #-} Request method code date path host mSize
          char = word8 . c2w
          space = char ' '
          word = takeWhile $ (/= ' ') . w2c
          num = (maybe 0 fst . BC.readInteger) `liftM` takeWhile (isDigit . w2c)
          num' = (maybe 0 fst . BC.readInt) `liftM` takeWhile (isDigit . w2c)
          host = do h <- BC.unpack `liftM` word
                    case ':' `elem` h of
                      False ->
                        return $ Host4 $ readAddress h
                      True ->
                        return $ Host6 $ readAddress h

          date = do day <- num'
                    char '/'
                    mon <- month
                    char '/'
                    year <- num
                    char ':'
                    return $ fromGregorian year mon day
          month = fromMaybe (error "Invalid month") <$>
                  flip Map.lookup months <$> 
                  takeWhile (isAlpha . w2c)
            where
              months = Map.fromList $ zip months' [1..]
              months' = map BC.pack months''
              months'' = ["Jan", "Feb", "Mar",
                          "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep",
                          "Oct", "Nov", "Dec"]
          eol = char '\n'
                         
parseFile :: LBC.ByteString -> [Request]
parseFile s = 
    case parseLine s of
      Done rest req ->
          req : parseFile rest
      Fail rest _ errMsg ->
          trace errMsg $
          parseFile $ LBC.tail rest

main :: IO ()
main = LBC.getContents >>=
       mapM_ print . parseFile
