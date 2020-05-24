{ name = "record-csv"
, dependencies =
  [ "console"
  , "effect"
  , "numbers"
  , "proxy"
  , "psci-support"
  , "string-parsers"
  , "test-unit"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
