module Record.CSV.OrderHelper
  ( pickHeaderOrder
  , sortColumns
  ) where

import Prelude
import Data.Either (Either(..), note)
import Data.List as L
import Data.Maybe (Maybe(..))
import Record.CSV.Error (CSVError(..))
import Record.CSV.Type (CSVLine, CSVResult, CSV)

pickHeaderOrder :: L.List String -> CSVLine -> CSVResult (L.List Int)
pickHeaderOrder phs hs = go phs
  where
  go L.Nil = Right L.Nil

  go (L.Cons rh rhs) = case L.elemIndex rh hs of
    Just i -> L.Cons i <$> go rhs
    Nothing -> Left <<< ColumnNameNotFound $ "Column name `" <> rh <> "` is not in the csv header."

sortColumns :: L.List Int -> CSV -> CSVResult CSV
sortColumns _ L.Nil = Right L.Nil

sortColumns ord xs =
  note Unreachable
    <<< map L.transpose
    <<< go
    $ ord
  where
  trx = L.transpose xs

  go L.Nil = Just L.Nil

  go (L.Cons i is) = L.Cons <$> L.index trx i <*> go is
