module Record.CSV.OrderHelper
  ( pickHeaderOrder
  , sortColumns
  ) where

import Prelude
import Data.Either (Either(..), note)
import Data.List as L
import Data.Maybe (Maybe(..))

pickHeaderOrder :: L.List String -> L.List String -> Either String (L.List Int)
pickHeaderOrder phs hs = go phs
  where
  go L.Nil = Right L.Nil

  go (L.Cons rh rhs) = case L.elemIndex rh hs of
    Just i -> L.Cons i <$> go rhs
    Nothing -> Left $ "Name `" <> rh <> "` is not in the header."

sortColumns :: L.List Int -> L.List (L.List String) -> Either String (L.List (L.List String))
sortColumns _ L.Nil = Right L.Nil

sortColumns ord xs =
  note "Column index not found."
    <<< map L.transpose
    <<< go
    $ ord
  where
  trx = L.transpose xs

  go L.Nil = Just L.Nil

  go (L.Cons i is) = L.Cons <$> L.index trx i <*> go is
