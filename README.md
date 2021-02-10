# purescript-record-csv

[![Build Status](https://travis-ci.com/hjmtql/purescript-record-csv.svg?branch=master)](https://travis-ci.com/hjmtql/purescript-record-csv)

CSV parser and printer for PureScript's record.

``` purescript
type Person =
    { name :: String
    , age :: Int
    , married :: Boolean
    }

parsePerson :: String -> CSVResult (List Person)
parsePerson = parseCSV

printPerson :: List Person -> String
printPerson = printCSV
```

## Parser Example

String value must require double quotations.
It is good to avoid troublesome (Maybe String, escape character).

| Type         | Example                        |
| ------------ | ------------------------------ |
| String       | "hello","","Say \\"hi\\".",... |
| Int          | 1,0,-1,...                     |
| Number       | -1.2,1,1.2e3,...               |
| Boolean      | true,false                     |
| Maybe String | "hello",,...                   |

## Column Order 

※ PureScript row type has no order, so it will be automatically treated as alphabetical order.

To specify the column order when printing, write code like below.

``` purescript
type Order
    = "name"
    : "age"
    ! "married"

ord :: SLProxy Order
ord = SLProxy

printPersonWithOrder :: List Person -> String
printPersonWithOrder = printCSVWithOrder ord
```

## Custom Type

Define instances of [`FromCSV`](src/Record/CSV/Parser/FromCSV.purs) and [`ToCSV`](src/Record/CSV/Printer/ToCSV.purs) to use custom types.

For example, sum type is as [test/CustomType.purs](test/CustomType.purs).
