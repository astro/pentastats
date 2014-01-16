{-# LANGUAGE MultiParamTypeClasses #-}
module Shared where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Convertible
import Data.List
import Data.Char (isDigit, isSpace, ord)
import Data.Hashable

padLeft :: [a] -> Int -> [a] -> [a]
padLeft xs l padding
    | length xs >= l = xs
    | otherwise = padLeft (padding ++ xs) l padding

readInt :: Num a => String -> Maybe a
readInt ('-':s) = (0 -) <$> readInt s
readInt s = readInt' 0 s

readInt' :: Num a => a -> String -> Maybe a
readInt' _ "" = Nothing
readInt' r (x : xs)
    | isDigit x = let i = fromIntegral $ ord x - ord '0'
                      i' = r * 10 + i
                  in seq i' $
                     case xs of
                       "" -> Just i'
                       _:_ -> readInt' i' xs
    | otherwise = Nothing

data Date = Date !Int !Int !Int
            deriving (Ord, Eq)
                     
instance Show Date where
    show (Date y m d) = intercalate "-"
                        [show y,
                         padLeft (show m) 2 "0",
                         padLeft (show d) 2 "0"]
                     
instance Convertible BC.ByteString Date where
    safeConvert b =
        case BC.unpack b of
          [y, y', y'', y''', 
           '-', m, m',
           '-', d, d'] ->
                  let Just year = readInt [y, y', y'', y''']
                      Just month = readInt [m, m']
                      Just day = readInt [d, d']
                  in Right $
                     Date year month day
          _ ->
              fail "Invalid date"
                         
instance Convertible Date BC.ByteString where
    safeConvert = Right . BC.pack . show

instance Hashable Date where
    hashWithSalt salt (Date y m d) = hashWithSalt salt (y, m, d)

data DayTime = DayTime Int Int Int
             deriving (Ord, Eq)

instance Show DayTime where
    show (DayTime h m s) = intercalate ":"
                           [padLeft (show h) 2 "0",
                            padLeft (show m) 2 "0",
                            padLeft (show s) 2 "0"]

instance Convertible BC.ByteString DayTime where
    safeConvert b =
        case BC.unpack b of
          [h, h', 
           ':', m, m',
           ':', s, s']
              | isDigit h && isDigit h' &&
                isDigit m && isDigit m' &&
                isDigit s && isDigit s' ->
                  let Just hour = readInt [h, h']
                      Just minute = readInt [m, m']
                      Just second = readInt [s, s']
                  in Right $
                     DayTime hour minute second
          _ ->
              fail "Invalid date"

instance Convertible DayTime BC.ByteString where
    safeConvert = Right . BC.pack . show


data Key = Key {
      kPath :: BC.ByteString,
      kDate :: Date,
      kHost :: BC.ByteString,
      kUserAgent :: BC.ByteString,
      kReferer :: BC.ByteString
    } deriving (Show, Eq, Ord)
           
instance Convertible BC.ByteString Key where
    safeConvert b =
        case BC.lines b of
          path : date : host : rest ->
              let userAgent =
                    case rest of
                      userAgent' : _ -> userAgent'
                      _ -> BC.empty
                  referer =
                    case rest of
                      _ : referer' : _ -> referer'
                      _ -> BC.empty
              in Key path <$>
                 safeConvert date <*>
                 pure host <*>
                 pure userAgent <*>
                 pure referer
          _ ->
              fail $ "Invalid key: " ++ show b

instance Convertible Key BC.ByteString where
    safeConvert key =
        do date <- safeConvert $ kDate key
           return $
                  BC.unlines [kPath key,
                              date,
                              kHost key,
                              kUserAgent key,
                              kReferer key]

data Value = Value {
      vCount :: Integer,
      vSize :: Integer,
      vToken :: BC.ByteString
    } deriving (Show, Eq)

instance Ord Value where
    compare v1 v2 = 
        case vToken v1 `compare` vToken v2 of
          EQ -> vSize v1 `compare` vSize v2
          ordering -> ordering

instance Convertible BC.ByteString Value where
    safeConvert b =
        case readNumbers b of
          ([count, size], token) ->
            Right $
            Value (max 1 count) (max 0 size) token
          ([size], token) ->
            Right $
            Value 1 (max 0 size) token
          _ -> fail $ "Invalid value: " ++ show b

      where readNumbers :: BC.ByteString -> ([Integer], BC.ByteString)
            readNumbers b =
              case BC.break (not . isDigit) b of
                (n, rest) | BC.null n &&
                            BC.length rest > 0 &&
                            BC.head rest == '-' ->
                  let rest' = BC.tail rest
                      ((n':ns), rest'') = readNumbers rest'
                  in (-n' : ns, rest'')
                (n, _) | BC.null n ->
                  ([], b)
                (n, rest) ->
                  let Just n' = readInt $ BC.unpack n
                      rest' = BC.dropWhile isSpace rest
                      (ns, rest'') = readNumbers rest'
                  in (n' : ns, rest'')

instance Convertible Value BC.ByteString where
    safeConvert (Value count size token) =
        Right $
        BC.unwords [BC.pack $ show count,
                    BC.pack $ show size,
                    token
                   ]
