module Test.Main where

import Prelude
import Effect (Effect)
import Test.Checker (checker)
import Test.Order (order)
import Test.OrderHelper (orderHelper)
import Test.Quick (tour)
import Test.Sum (sum)

main :: Effect Unit
main = do
  tour
  sum
  order
  orderHelper
  checker
