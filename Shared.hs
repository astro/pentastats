{-# LANGUAGE MultiParamTypeClasses #-}
module Shared where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Convertible
import Data.List
import Data.Char (isDigit, ord)

padLeft :: [a] -> Int -> [a] -> [a]
padLeft xs l padding
    | length xs >= l = xs
    | otherwise = padLeft (padding ++ xs) l padding

readInt :: String -> Maybe Int
readInt "" = Nothing
readInt (x : xs)
    | isDigit x = let i = ord x - ord '0'
                  in seq i $
                     case xs of
                       "" -> Just i
                       _:_ -> ((i * 10) +) <$> readInt xs
    | otherwise = Nothing

data Date = Date Int Int Int
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
      kUserAgent :: BC.ByteString
    } deriving (Show, Eq, Ord)
           
instance Convertible BC.ByteString Key where
    safeConvert b =
        case BC.lines b of
          path : date : host : rest ->
              let userAgent = case rest of
                                [userAgent'] -> userAgent'
                                _ -> BC.empty
              in Key path <$>
                 safeConvert date <*>
                 pure host <*>
                 pure userAgent
          _ ->
              fail $ "Invalid key: " ++ show b

instance Convertible Key BC.ByteString where
    safeConvert key =
        do date <- safeConvert $ kDate key
           return $
                  BC.unlines [kPath key,
                              date,
                              kHost key,
                              kUserAgent key]

newtype Values = Values [Value]

instance Convertible BC.ByteString Values where
    safeConvert = (Values <$>) . mapM safeConvert . BC.lines

instance Convertible Values BC.ByteString where
    safeConvert (Values values) =
        BC.unlines <$> mapM safeConvert values

data Value = Value {
      vSize :: Int,
      vToken :: BC.ByteString
    } deriving (Eq)

instance Ord Value where
    compare v1 v2 = 
        case vToken v1 `compare` vToken v2 of
          EQ -> vSize v1 `compare` vSize v2
          ordering -> ordering

instance Convertible BC.ByteString Value where
    safeConvert b =
        let (b', b'') = BC.break (== ' ') b
        in case readInt $ BC.unpack b' of
             Just size -> Right $ Value size b''
             Nothing -> fail "Invalid value"

instance Convertible Value BC.ByteString where
    safeConvert (Value size token) =
        Right $
        BC.concat [BC.pack $ show size,
                   BC.singleton ' ',
                   token
                  ]
