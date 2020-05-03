module Record.CSV.Parser.ParseAsString
  ( parseAsString
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either)
import Data.List as L
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodeUnits as CU
import Text.Parsing.StringParser.Combinators (many, sepBy)

parseAsString :: String -> Either ParseError (L.List (L.List String))
parseAsString = runParser csv

csv :: Parser (L.List (L.List String))
csv = sepBy line (CU.char '\n')

line :: Parser (L.List String)
line = sepBy cell (CU.char ',')

cell :: Parser String
cell =
  fromCharArray
    <<< A.fromFoldable
    <$> (quated <|> direct)

direct :: Parser (L.List Char)
direct = many (CU.noneOf [ ',', '\n' ])

-- NOTE: take double quotations to handle Maybe String
quated :: Parser (L.List Char)
quated = do
  open <- CU.char '"'
  inner <- many (special <|> CU.noneOf [ '"' ])
  close <- CU.char '"'
  pure $ L.singleton open <> inner <> L.singleton close

-- REVIEW
special :: Parser Char
special =
  CU.char '\\'
    *> ( control
          <|> escape
      )

control :: Parser Char
control =
  to' "127" '\x7f'
    <|> to' "14" '\x0e'
    <|> to' "15" '\x0f'
    <|> to' "16" '\x10'
    <|> to' "17" '\x11'
    <|> to' "18" '\x12'
    <|> to' "19" '\x13'
    <|> to' "20" '\x14'
    <|> to' "21" '\x15'
    <|> to' "22" '\x16'
    <|> to' "23" '\x17'
    <|> to' "24" '\x18'
    <|> to' "25" '\x19'
    <|> to' "26" '\x1a'
    <|> to' "27" '\x1b'
    <|> to' "28" '\x1c'
    <|> to' "29" '\x1d'
    <|> to' "30" '\x1e'
    <|> to' "31" '\x1f'
    <|> to '0' '\x00'
    <|> to '1' '\x01'
    <|> to '2' '\x02'
    <|> to '3' '\x03'
    <|> to '4' '\x04'
    <|> to '5' '\x05'
    <|> to '6' '\x06'
    <|> to 'a' '\x07'
    <|> to 'b' '\x08'
    <|> to 't' '\x09'
    <|> to 'n' '\x0a'
    <|> to 'v' '\x0b'
    <|> to 'f' '\x0c'
    <|> to 'r' '\x0d'

escape :: Parser Char
escape =
  to '\\' '\x5c'
    <|> to '"' '\x22'

to :: Char -> Char -> Parser Char
to f t = CU.char f *> pure t

to' :: String -> Char -> Parser Char
to' f t = CU.string f *> pure t
