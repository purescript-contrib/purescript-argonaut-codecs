{ name = "argonaut-codecs"
, license = "MIT"
, repository =
    "https://github.com/purescript-contrib/purescript-argonaut-codecs"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
