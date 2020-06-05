module Test.ReadValue where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe)
import Record.CSV.Parser (parseCSV)
import Record.CSV.Printer (printCSV)
import Test.QuickCheck ((<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

type Sample
  = { string :: String
    , int :: Int
    , number :: Number
    , boolean :: Boolean
    , maybeString :: Maybe String
    , maybeInt :: Maybe Int
    , maybeNumber :: Maybe Number
    , maybeBoolean :: Maybe Boolean
    }

readValue :: TestSuite
readValue =
  suite "read value" do
    test "quickcheck" do
      quickCheck \(xs :: L.List Sample) -> Right xs == parseCSV (printCSV xs) <?> show xs
