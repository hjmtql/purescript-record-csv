module Test.Main where

import Prelude
import Effect (Effect)
import Test.ColumnOrder (colunmOrder)
import Test.CustomType (sumType)
import Test.OrderHelper (orderHelper)
import Test.Overflow (overflow)
import Test.ReadValue (readValue)
import Test.ShapeChecker (shapeChecker)
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    readValue
    sumType
    colunmOrder
    orderHelper
    shapeChecker
    overflow
