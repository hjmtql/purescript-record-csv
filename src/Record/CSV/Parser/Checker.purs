module Record.CSV.Parser.Checker
  ( sameLength
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List as L

sameLength :: L.List (L.List String) -> Either String (L.List (L.List String))
sameLength L.Nil = Right L.Nil

sameLength (L.Cons h tail) = L.Cons h <$> check tail
  where
  len = L.length h

  check L.Nil = Right L.Nil

  check (L.Cons x xs)
    | L.length x == len = L.Cons <$> Right x <*> check xs
    | otherwise = Left "Different column length."
