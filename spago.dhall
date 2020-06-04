{ name = "record-csv"
, dependencies =
  [ "numbers"
  , "record"
  , "string-parsers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
