module Record.CSV.Parser.ParseAsString
  ( parseAsString
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either)
import Data.List as L
import Data.String.CodeUnits (fromCharArray)
import Record.CSV.Type (CSV, CSVLine)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodeUnits as CU
import Text.Parsing.StringParser.Combinators (many, sepBy, sepEndBy)

parseAsString :: String -> Either ParseError CSV
parseAsString = runParser csv

csv :: Parser CSV
csv = sepEndBy line (CU.char '\n')

line :: Parser CSVLine
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
    *> ( escape
          <|> control
      )

control :: Parser Char
control =
  CU.char '0' *> pure '\x00'
    <|> ( CU.char '1'
          *> ( CU.char '2' *> CU.char '7' *> pure '\x7f'
                <|> (CU.char '4' *> pure '\x0e')
                <|> (CU.char '5' *> pure '\x0f')
                <|> (CU.char '6' *> pure '\x10')
                <|> (CU.char '7' *> pure '\x11')
                <|> (CU.char '8' *> pure '\x12')
                <|> (CU.char '9' *> pure '\x13')
                <|> (pure '\x01')
            )
      )
    <|> ( CU.char '2'
          *> ( CU.char '0' *> pure '\x14'
                <|> (CU.char '1' *> pure '\x15')
                <|> (CU.char '2' *> pure '\x16')
                <|> (CU.char '3' *> pure '\x17')
                <|> (CU.char '4' *> pure '\x18')
                <|> (CU.char '5' *> pure '\x19')
                <|> (CU.char '6' *> pure '\x1a')
                <|> (CU.char '7' *> pure '\x1b')
                <|> (CU.char '8' *> pure '\x1c')
                <|> (CU.char '9' *> pure '\x1d')
                <|> (pure '\x02')
            )
      )
    <|> ( CU.char '3'
          *> ( CU.char '0' *> pure '\x1e'
                <|> (CU.char '1' *> pure '\x1f')
                <|> (pure '\x03')
            )
      )
    <|> (CU.char '4' *> pure '\x04')
    <|> (CU.char '5' *> pure '\x05')
    <|> (CU.char '6' *> pure '\x06')
    <|> (CU.char 'a' *> pure '\x07')
    <|> (CU.char 'b' *> pure '\x08')
    <|> (CU.char 't' *> pure '\x09')
    <|> (CU.char 'n' *> pure '\x0a')
    <|> (CU.char 'v' *> pure '\x0b')
    <|> (CU.char 'f' *> pure '\x0c')
    <|> (CU.char 'r' *> pure '\x0d')

escape :: Parser Char
escape =
  CU.char '\\' *> pure '\x5c'
    <|> (CU.char '"' *> pure '\x22')
