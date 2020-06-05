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

sameLength a@(L.Cons h tail)
  | L.all (eq (L.length h)) (map L.length tail) = Right a
  | otherwise = Left $ DifferntColumnLength "Column lengths are not the same."
