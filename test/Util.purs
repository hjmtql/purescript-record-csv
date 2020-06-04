module Test.Util where

import Prelude
import Data.List (List, fromFoldable)

fromFoldableNest :: forall a. Array (Array a) -> List (List a)
fromFoldableNest = fromFoldable <<< map fromFoldable
