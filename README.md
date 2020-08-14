# Argonaut Codecs

[![CI](https://github.com/purescript-contrib/purescript-argonaut-codecs/workflows/CI/badge.svg?branch=master)](https://github.com/purescript-contrib/purescript-argonaut-codecs/actions?query=workflow%3ACI+branch%3Amaster)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-codecs.svg)](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-codecs/badge)](http://pursuit.purescript.org/packages/purescript-argonaut-codecs)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](http://github.com/thomashoneyman)

[Argonaut](https://github.com/purescript-contrib/purescript-argonaut) is a collection of libraries for working with JSON in PureScript. `argonaut-codecs` provides codecs based on the `EncodeJson` and `DecodeJson` type classes, along with instances for common data types and combinators for encoding and decoding `Json` values.

You may also be interested in these other libraries from the Argonaut ecosystem:

- [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core) defines the `Json` type, along with basic parsing, printing, and folding functions
- [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals) defines prisms, traversals, and zippers for the `Json` type.
- [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic) supports generic encoding and decoding for any type with a `Generic` instance
- [purescript-codec-argonaut](https://github.com/garyb/purescript-codec-argonaut) supports an alternative approach for codecs, which are based on profunctors instead of type classes

The quick start will get you up and running with the basics of `argonaut-codecs`. For a deeper dive, please see [the full documentation for this library](./docs), which includes an in-depth tutorial.

## Installation

Install with [Spago](https://github.com/purescript/spago):

```sh
spago install argonaut-codecs
```

or install as part of the [Argonaut](https://github.com/purescript-contrib/purescript-argonaut) bundle:

```sh
spago install argonaut
```

## Quick start

Use `encodeJson` to encode PureScript data types as `Json` and `decodeJson` to decode `Json` into PureScript types, with helpful error messages if decoding fails.

```purs
type User = { name :: String, age :: Maybe Int }

-- We get encoding and decoding for free because of the `EncodeJson` instances
-- for records, strings, integers, and `Maybe`, along with many other common
-- PureScript types.

userToJson :: User -> Json
userToJson = encodeJson

userFromJson :: Json -> Either JsonDecodeError User
userFromJson = decodeJson
```

In a REPL we can see these functions in action:

```text
> type User = { name :: String, age :: Maybe Int }
> user = { name: "Tom", age: Just 25 }
> stringify (encodeJson user)
"{\"name\":\"Tom\",\"age\":25}"

> (decodeJson =<< parseJson """{ "name": "Tom", "age": 25 }""") :: Either JsonDecodeError User
Right { name: "Tom", age: Just 25 }

> res = (decodeJson =<< parseJson """{ "name": "Tom" }""") :: Either JsonDecodeError User
> res
Left (AtKey "age" MissingValue)

# You can print errors
> lmap printJsonDecodeError res
Left "An error occurred while decoding a JSON value:\n  At object key 'age':\n  No value was found."
```

## Documentation

You can find `argonaut-codecs` documentation in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-argonaut-codecs).
2. The tutorial and other written documentation is kept in [the docs directory](./docs).
3. Additional usage examples can be found in [the PureScript Cookbook](https://github.com/JordanMartinez/purescript-cookbook)

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-argonaut-codecs/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to `argonaut-codecs` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-argonaut-codecs/issues) issue. We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./.github/CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
