# purescript-record-csv

Provides parse and print CSV functions for PureScript's record.

``` purescript
type Person =
    { name :: String
    , age :: Int
    , married :: Boolean
    }

parsePerson :: String -> Either String (List Person)
parsePerson = parseCSV

printPerson :: List Person -> String
printPerson = printCSV
```

## Column Order 

※ PureScript row type has no order, so it will automatically be treated as alphabetical order.

To specify the column order when you print, write code like below.

``` purescript
type Order
    = "name"
    : "age"
    :| "married"

ord :: SLProxy Order
ord = SLProxy

printPersonWithOrder :: List Person -> Either String String
printPersonWithOrder = printCSVWithOrder ord
```

## Parser Cautions

- String value must require double quotations -> good to avoid troublesome (ex. Maybe String, escape)
- CSV end with '\n' can not be parsed -> fixable
