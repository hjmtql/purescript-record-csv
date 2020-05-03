module Test.Sum where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Effect (Effect)
import Record.CSV.Parser (parseCSV)
import Record.CSV.Parser.FromCSV (class FromCSV)
import Record.CSV.Printer (printCSV)
import Record.CSV.Printer.ToCSV (class ToCSV)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

data Animal
  = Cat
  | Dog
  | Bird

instance toCSVAnimal :: ToCSV Animal where
  toCSV Cat = "cat"
  toCSV Dog = "dog"
  toCSV Bird = "bird"

instance fromCSVAnimal :: FromCSV Animal where
  fromCSV "cat" = Right Cat
  fromCSV "dog" = Right Dog
  fromCSV "bird" = Right Bird
  fromCSV _ = Left "Faild to parse Animal."

derive instance eqAnimal :: Eq Animal

derive instance generic :: Generic Animal _

instance showAnimal :: Show Animal where
  show = genericShow

type Pet
  = { name :: String
    , kind :: Animal
    }

xs :: L.List Pet
xs =
  L.fromFoldable
    [ { name: "chiro", kind: Cat }
    , { name: "hachi", kind: Dog }
    ]

xs' :: String
xs' = "kind,name\ncat,\"chiro\"\ndog,\"hachi\""

sum :: Effect Unit
sum =
  runTest do
    suite "sum type" do
      test "print" do
        Assert.equal (printCSV xs) xs'
      test "parse" do
        Assert.equal (parseCSV xs') (Right xs)
