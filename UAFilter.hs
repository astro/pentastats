module UAFilter (loadFilters) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.Char8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Text.Regex.PCRE.ByteString as Re


data Rule = Rule BC.ByteString T.Text
          deriving (Show)

filtersParser :: Parser [Rule]
filtersParser =
    eof <|>
    whitespace <|>
    comment <|>
    rule
    where eof = endOfInput *> pure []
          whitespace = space *>
                       filtersParser
          comment = char '#' *>
                    takeTill (== '\n') *>
                    filtersParser
          rule = do r <- flip Rule <$>
                         (char '"' *>
                          (decodeUtf8 <$> takeTill (== '\"')) <* 
                          char '"') <*>
                         (skipMany1 space *>
                          takeTill (== '\n'))
                    (r :) <$> filtersParser

type UAFilter = BC.ByteString -> IO (Maybe T.Text)

data CompiledRule = CompiledRule Re.Regex T.Text

uaFilter :: [CompiledRule] -> UAFilter
uaFilter [] _ = return Nothing
uaFilter ((CompiledRule regex name):rules) ua =
    Re.execute regex ua >>=
    either (error . snd)
    (maybe (uaFilter rules ua) (const $ return $ Just name))

rulesToFilter :: [Rule] -> IO UAFilter
rulesToFilter rules =
    uaFilter <$> 
    forM rules (\(Rule regex name) ->
                    Re.compile Re.compBlank Re.execBlank regex >>=
                      either (error . snd) (return . (flip CompiledRule name))
               )

loadFilters :: IO UAFilter
loadFilters =
    parseOnly filtersParser <$> BC.readFile "user-agent.filters" >>=
    either error rulesToFilter
