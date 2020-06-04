module Record.CSV.Type
  ( CSV
  , CSVLine
  , CSVResult
  ) where

import Data.Either (Either)
import Data.List (List)
import Record.CSV.Error (CSVError)

type CSV
  = List CSVLine

type CSVLine
  = List String

type CSVResult
  = Either CSVError
