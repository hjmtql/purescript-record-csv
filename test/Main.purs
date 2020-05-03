module Test.Main where

import Prelude
import Effect (Effect)
import Test.Quick (tour)
import Test.Sum (sum)

main :: Effect Unit
main = do
  tour
  sum
