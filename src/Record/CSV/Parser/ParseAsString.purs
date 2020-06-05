module Record.CSV.Parser.ParseAsString
  ( parseAsString
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Trampoline (runTrampoline)
import Data.Array as A
import Data.Either (Either)
import Data.List as L
import Data.String.CodeUnits (fromCharArray)
import Record.CSV.Type (CSV, CSVLine)
import Text.Parsing.Parser (ParseError, ParserT, runParserT)
import Text.Parsing.Parser.Combinators (sepBy, sepEndBy)
import Text.Parsing.Parser.String (char, noneOf)

type StringParserT
  = ParserT String

-- NOTE: use trampoline to avoid stack overflow
-- https://github.com/purescript-contrib/purescript-parsing/issues/34
parseAsString :: String -> Either ParseError CSV
parseAsString s = runTrampoline $ runParserT s csv

csv :: forall m. Monad m => StringParserT m CSV
csv = sepEndBy line (char '\n')

line :: forall m. Monad m => StringParserT m CSVLine
line = sepBy cell (char ',')

cell :: forall m. Monad m => StringParserT m String
cell =
  fromCharArray
    <<< A.fromFoldable
    <$> (quated <|> direct)

direct :: forall m. Monad m => StringParserT m (L.List Char)
direct = L.many (noneOf [ ',', '\n' ])

-- NOTE: take double quotations to handle Maybe String
quated :: forall m. Monad m => StringParserT m (L.List Char)
quated = do
  open <- char '"'
  inner <- L.many (special <|> noneOf [ '"' ])
  close <- char '"'
  pure $ L.singleton open <> inner <> L.singleton close

-- REVIEW
special :: forall m. Monad m => StringParserT m Char
special =
  char '\\'
    *> ( escape
          <|> control
      )

control :: forall m. Monad m => StringParserT m Char
control =
  char '0' *> pure '\x00'
    <|> ( char '1'
          *> ( char '2' *> char '7' *> pure '\x7f'
                <|> (char '4' *> pure '\x0e')
                <|> (char '5' *> pure '\x0f')
                <|> (char '6' *> pure '\x10')
                <|> (char '7' *> pure '\x11')
                <|> (char '8' *> pure '\x12')
                <|> (char '9' *> pure '\x13')
                <|> (pure '\x01')
            )
      )
    <|> ( char '2'
          *> ( char '0' *> pure '\x14'
                <|> (char '1' *> pure '\x15')
                <|> (char '2' *> pure '\x16')
                <|> (char '3' *> pure '\x17')
                <|> (char '4' *> pure '\x18')
                <|> (char '5' *> pure '\x19')
                <|> (char '6' *> pure '\x1a')
                <|> (char '7' *> pure '\x1b')
                <|> (char '8' *> pure '\x1c')
                <|> (char '9' *> pure '\x1d')
                <|> (pure '\x02')
            )
      )
    <|> ( char '3'
          *> ( char '0' *> pure '\x1e'
                <|> (char '1' *> pure '\x1f')
                <|> (pure '\x03')
            )
      )
    <|> (char '4' *> pure '\x04')
    <|> (char '5' *> pure '\x05')
    <|> (char '6' *> pure '\x06')
    <|> (char 'a' *> pure '\x07')
    <|> (char 'b' *> pure '\x08')
    <|> (char 't' *> pure '\x09')
    <|> (char 'n' *> pure '\x0a')
    <|> (char 'v' *> pure '\x0b')
    <|> (char 'f' *> pure '\x0c')
    <|> (char 'r' *> pure '\x0d')

escape :: forall m. Monad m => StringParserT m Char
escape =
  char '\\' *> pure '\x5c'
    <|> (char '"' *> pure '\x22')
