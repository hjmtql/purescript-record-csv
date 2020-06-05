module Record.CSV.Error
  ( CSVError(..)
  , csvError
  , fromParseError
  ) where

import Prelude
import Text.Parsing.Parser (ParseError)

-- import Text.Parsing.StringParser (ParseError)
data CSVError
  = NoLine String
  | ReadValueFailure String
  | DifferntColumnLength String
  | ColumnNameNotFound String
  | ParseAsStringFailure String
  -- NOTE: Unreachable represents an error that could not be type checked.
  | Unreachable

derive instance eqCSVError :: Eq CSVError

csvError :: String -> CSVError
csvError = ReadValueFailure

fromParseError :: ParseError -> CSVError
fromParseError = ParseAsStringFailure <<< show

instance showCSVError :: Show CSVError where
  show (NoLine s) = s
  show (ReadValueFailure s) = s
  show (DifferntColumnLength s) = s
  show (ColumnNameNotFound s) = s
  show (ParseAsStringFailure s) = s
  show Unreachable = "Unexpected error."
