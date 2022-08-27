{ name = "record-csv"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "prelude"
  , "record"
  , "string-parsers"
  , "strings"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
