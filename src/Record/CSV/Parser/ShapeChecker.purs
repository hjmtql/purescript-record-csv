module Record.CSV.Parser.ShapeChecker
  ( sameLength
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Record.CSV.Error (CSVError(..))
import Record.CSV.Type (CSV, CSVResult)

sameLength :: CSV -> CSVResult CSV
sameLength L.Nil = Right L.Nil

sameLength (L.Cons h tail) = L.Cons h <$> check tail
  where
  len = L.length h

  check L.Nil = Right L.Nil

  check (L.Cons x xs)
    | L.length x == len = L.Cons <$> Right x <*> check xs
    | otherwise = Left $ DifferntColumnLength "Column lengths are not the same."
