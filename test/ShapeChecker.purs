module Test.ShapeChecker where

import Prelude
import Data.Either (Either(..), isLeft)
import Record.CSV.Parser.ShapeChecker (sameLength)
import Record.CSV.Type (CSV)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Util (fromFoldableNest)

validCSV :: CSV
validCSV =
  fromFoldableNest
    [ [ "age", "name", "married" ]
    , [ "20", "paul", "false" ]
    , [ "40", "john", "true" ]
    ]

invalidCSV :: CSV
invalidCSV =
  fromFoldableNest
    [ [ "age", "name", "married" ]
    , [ "20", "paul", "false" ]
    , [ "40", "john" ]
    ]

shapeChecker :: TestSuite
shapeChecker =
  suite "csv shape check" do
    test "same length" do
      Assert.equal (Right validCSV) (sameLength validCSV)
    test "differnt length" do
      Assert.assert "should fail" $ isLeft (sameLength invalidCSV)
