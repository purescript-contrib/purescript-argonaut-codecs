{ name = "argonaut-codecs"
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-argonaut-codecs"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "identity"
  , "integers"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
