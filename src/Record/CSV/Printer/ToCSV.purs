module Record.CSV.Printer.ToCSV
  ( class ToCSV
  , toCSV
  ) where

import Prelude
import Data.Maybe (Maybe(..))

class ToCSV a where
  toCSV :: a -> String

-- NOTE: add double quotation for String
instance toCSVString :: ToCSV String where
  toCSV = show

instance toCSVInt :: ToCSV Int where
  toCSV = show

instance toCSVNumber :: ToCSV Number where
  toCSV = show

instance toCSVBoolean :: ToCSV Boolean where
  toCSV = show

instance toCSVMaybe :: ToCSV a => ToCSV (Maybe a) where
  toCSV (Just a) = toCSV a
  toCSV Nothing = ""
