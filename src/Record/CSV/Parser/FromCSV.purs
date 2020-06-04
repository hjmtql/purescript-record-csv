module Record.CSV.Parser.FromCSV
  ( class FromCSV
  , fromCSV
  ) where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as N
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Record.CSV.Error (csvError)
import Record.CSV.Type (CSVResult)

class FromCSV a where
  fromCSV :: String -> CSVResult a

-- NOTE: require double quotations for String
instance fromCSVString :: FromCSV String where
  fromCSV s = case A.head cs, A.last cs of
    Just '"', Just '"' -> Right <<< fromCharArray <<< fromMaybe [] <<< (A.init <=< A.tail) $ cs
    _, _ -> Left <<< csvError $ "Cound not read " <> s <> " as String."
    where
    cs = toCharArray s

instance fromCSVInt :: FromCSV Int where
  fromCSV s = case I.fromString s of
    Just i -> Right i
    Nothing -> Left $ csvError $ "Cound not read " <> s <> " as Int."

instance fromCSVNumber :: FromCSV Number where
  fromCSV s = case N.fromString s of
    Just n -> Right n
    Nothing -> Left $ csvError $ "Cound not read " <> s <> " as Number."

instance fromCSVBoolean :: FromCSV Boolean where
  fromCSV "true" = Right true
  fromCSV "false" = Right false
  fromCSV s = Left $ csvError $ "Cound not read " <> s <> " as Boolean."

instance fromCSVMaybe :: FromCSV a => FromCSV (Maybe a) where
  fromCSV "" = Right Nothing
  fromCSV s = Just <$> fromCSV s
