module Test.Checker where

import Prelude
import Data.Either (Either(..), isLeft)
import Data.List as L
import Effect (Effect)
import Record.CSV.Parser.Checker (sameLength)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

validCSV :: (L.List (L.List String))
validCSV =
  fromFoldableNest
    [ [ "age", "name", "married" ]
    , [ "20", "paul", "false" ]
    , [ "40", "john", "true" ]
    ]

invalidCSV :: (L.List (L.List String))
invalidCSV =
  fromFoldableNest
    [ [ "age", "name", "married" ]
    , [ "20", "paul", "false" ]
    , [ "40", "john" ]
    ]

fromFoldableNest :: forall a. Array (Array a) -> L.List (L.List a)
fromFoldableNest = L.fromFoldable <<< map L.fromFoldable

checker :: Effect Unit
checker =
  runTest do
    suite "csv checker" do
      test "same length" do
        Assert.equal (Right validCSV) (sameLength validCSV)
      test "differnt length" do
        Assert.assert "should fail" $ isLeft (sameLength invalidCSV)
