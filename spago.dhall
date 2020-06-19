{ name = "argonaut-codecs"
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
