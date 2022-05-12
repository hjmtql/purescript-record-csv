{ name = "record-csv"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "foldable-traversable"
  , "free"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "parsing"
  , "prelude"
  , "record"
  , "strings"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
