module Test.OrderHelper where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Record.CSV.OrderHelper (pickHeaderOrder, sortColumns)
import Record.CSV.Type (CSV, CSVLine)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Util (fromFoldableNest)

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

orderHelper :: TestSuite
orderHelper =
  suite "order helper" do
    test "pickHeaderOrder" do
      Assert.equal (Right ord) (pickHeaderOrder rowHeader header)
    test "sortColumns" do
      Assert.equal (Right rowSortedValues) (sortColumns ord values)
  where
  ord = L.fromFoldable [ 1, 0, 2 ]
