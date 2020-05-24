module Test.Order where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Effect (Effect)
import Record.CSV.Parser (parseCSV)
import Record.CSV.Printer (printCSVWithOrder)
import Record.CSV.Printer.SList (type (:), type (:|), SLProxy(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- NOTE: RowList is automatically treated as alphabetical order
type Person
  = { name :: String
    , age :: Int
    , married :: Boolean
    }

type Order
  = "name"
      : "age"
      :| "married"

ord :: SLProxy Order
ord = SLProxy

xs :: L.List Person
xs =
  L.fromFoldable
    [ { name: "paul", age: 20, married: false }
    , { name: "john", age: 40, married: true }
    ]

xs' :: String
xs' =
  """name,age,married
"paul",20,false
"john",40,true"""

order :: Effect Unit
order =
  runTest do
    suite "handle order" do
      test "print" do
        Assert.equal (Right xs') (printCSVWithOrder xs ord)
      test "parse" do
        Assert.equal (Right xs) (parseCSV xs')
