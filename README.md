# purescript-argonaut-codecs

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-codecs.svg)](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases)
[![Build status](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-codecs/badge)](http://pursuit.purescript.org/packages/purescript-argonaut-codecs/)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-lightgrey.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

`EncodeJson` and `DecodeJson` classes and instances, useful combinators for encoding and decoding `Json` values.

## Installation

```shell
bower install purescript-argonaut-codecs
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-codecs).

## Example

Using [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core) we can build a simple `Json` object:

```purescript
someObject =
  let
    objects =
      [ jsonSingletonObject "bar" (fromString "a")
      , jsonSingletonObject "bar" (fromString "b")
      ]
  in
    fromObject $ Object.fromFoldable [ Tuple "foo" (fromArray objects) ]
```

The `decodeJson`, `.:`, `.:?`, and `.!=` functions provided in this module make it straightforward to decode this JSON to a custom data type and serialize that custom data type back to JSON:

```purescript
newtype MyType = MyType
  { foo :: String
  , bar :: Maybe Int
  , baz :: Boolean
  }

-- create a `DecodeJson` instance
instance decodeJsonMyType :: DecodeJson MyType where
  decodeJson json = do
    x <- decodeJson json
    foo <- x .: "foo" -- mandatory field
    bar <- x .:? "bar" -- optional field
    baz <- x .:? "baz" .!= false -- optional field with default value of `false`
    pure $ MyType { foo, bar, baz }

-- or pass a function
decodeMyTypes :: Json -> Either String (Array MyType)
decodeMyTypes json = do
  x <- decodeJson json
  arr <- x .: "myTypes"
  for arr decodeJson

-- create a `EncodeJson` instance
instance encodeJsonMyType :: EncodeJson MyType where
  encodeJson (MyType x) =
    "foo" := x.foo 
      ~> "bar" :=? x.bar 
      ~>? "baz" := x.baz -- optional field
      ~> jsonEmptyObject
```

## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/.github/contributing.md) to get started and see helpful related resources.
