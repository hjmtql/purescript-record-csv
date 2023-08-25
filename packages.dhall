let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230824/packages.dhall
        sha256:9950996d73eee0e7ad15e00d28e04bc97a1fb2fda2012c3a60953e23558e1c5f

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
