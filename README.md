# purescript-argonaut-codecs

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-codecs.svg)](https://github.com/slamdata/purescript-argonaut-codecs/releases)
[![Build Status](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs)
[![Maintainer: slamdata](https://img.shields.io/badge/maintainer-slamdata-lightgrey.svg)](http://github.com/slamdata)

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
someObject = fromObject (StrMap.fromFoldable [
                Tuple "foo" (fromArray [
                  jsonSingletonObject "bar" (fromString "a"),
                  jsonSingletonObject "bar" (fromString "b")
                ])
              ])
```

The `decodeJson` and `.?` functions provided in this module make it straightforward to interrogate the `Json` object:

```purescript
main =
  log $ show $ getBars someObject

getBars :: Json -> Either String (Array String)
getBars json = do
  obj <- decodeJson json
  foo <- obj .? "foo"
  for foo \itemJson -> do
    itemObj <- decodeJson itemJson
    itemObj .? "bar"
```
