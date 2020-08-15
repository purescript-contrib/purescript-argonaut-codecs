{ name = "argonaut-codecs"
, license = "MIT"
, repository =
    "https://github.com/purescript-contrib/purescript-argonaut-codecs"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "assert"
  , "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "identity"
  , "integers"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "psci-support"
  , "quickcheck"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
