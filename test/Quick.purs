module Test.Quick where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe)
import Effect (Effect)
import Record.CSV.Parser (parseCSV)
import Record.CSV.Printer (printCSV)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
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

tour :: Effect Unit
tour =
  runTest do
    suite "basic type" do
      test "quickcheck" do
        quickCheck \(xs :: L.List Sample) -> Right xs == parseCSV (printCSV xs)
