# purescript-record-csv

Print and parse CSV functions for PureScript's record.

``` purescript
type Person =
    { name :: String
    , age :: Int
    }

printPerson :: List Person -> String
printPerson = printCSV

parsePerson :: String -> Either String (List Person)
parsePerson = parseCSV
```

## Parser Cautions

- Fixed column order (forsed a to z) -> hard to handle
- String value must require double quotations -> good to avoid troublesome (ex. Maybe String, escape)
- CSV end with '\n' can not be parsed -> fixable

※ Print function is useful aside from parse function 😅
