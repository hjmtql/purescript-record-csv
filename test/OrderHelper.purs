module Test.OrderHelper where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Effect (Effect)
import Record.CSV.OrderHelper (pickHeaderOrder, sortColumns)
import Record.CSV.Type (CSV, CSVLine)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

header :: CSVLine
header =
  L.fromFoldable
    [ "name"
    , "age"
    , "married"
    ]

-- NOTE: alphabetical
rowHeader :: CSVLine
rowHeader =
  L.fromFoldable
    [ "age"
    , "name"
    , "married"
    ]

values :: CSV
values =
  fromFoldableNest
    [ [ "paul", "20", "false" ]
    , [ "john", "40", "true" ]
    ]

rowSortedValues :: CSV
rowSortedValues =
  fromFoldableNest
    [ [ "20", "paul", "false" ]
    , [ "40", "john", "true" ]
    ]

fromFoldableNest :: forall a. Array (Array a) -> L.List (L.List a)
fromFoldableNest = L.fromFoldable <<< map L.fromFoldable

orderHelper :: Effect Unit
orderHelper =
  runTest do
    suite "orderHelper" do
      test "pickHeaderOrder" do
        Assert.equal (Right ord) (pickHeaderOrder rowHeader header)
      test "sortColumns" do
        Assert.equal (Right rowSortedValues) (sortColumns ord values)
  where
  ord = L.fromFoldable [ 1, 0, 2 ]
