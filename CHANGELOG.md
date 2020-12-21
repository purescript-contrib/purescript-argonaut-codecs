# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

New features:

- Added support for decoding missing record fields to `Nothing` (#93).

Bugfixes:

Other improvements:

## [v7.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v7.0.0) - 2020-06-19

This release introduces a few major changes:

**Introducing typed errors**

This release introduces a shift in the way this library handles errors. Previously, errors were reported as `String` values when decoding. Now, errors are reported as `JsonDecodeError` values, which provide much richer information about what errors have occurred. This brings this library into line with `purescript-codec-argonaut`, which also uses typed errors.

There are new functions to help work with this error type:

- `printJsonDecodeError :: JsonDecodeError -> String` can be used to recover a string error from a typed error
- `parseJson :: String -> Either JsonDecodeError Json` can be used instead of `Data.Argonaut.Parser.jsonParser` if you need to parse a `Json` value from a JSON string. It uses `jsonParser` under the hood, but provides a typed error instead of a string error.

Implemented in #73 and relevant documentation updated in #80.

**Add encoding and decoding functions without type classes**

Sometimes it is useful to be able to use the encoders and decoders defined in this library without needing the type classes. If you want to use the functions directly without a type class, they can now be found in the `Data.Argonaut.Decode.Decoders` and `Data.Argonaut.Encode.Encoders` modules.

Implemented in #74.

**Removed deprecated functions and operators**

A number of functions and operators were deprecated in the last release. Their removal doesn't change the functionality of this library -- as noted in the prior release, all deprecated operators have functionally-equivalent alternatives. For example:

- `.?` -> `.:`
- `.??` -> `.:?`
- `.?!` -> `.:!`

Implemented in #82.

**Migrate the library to use Spago**

This is a purely internal change, but the `purescript-argonaut-codecs` library now uses Spago internally to manage dependencies and the overall build. Over time the purescript-contrib organization will shift to use Spago instead of Pulp + Bower.

Implemented in #81.

## [v6.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v6.1.0) - 2020-05-08

This release includes two small improvements to the library:

* Adds new instances for `NonEmptyArray` and `NonEmptyList` (#61) and `Identity` (#54)
* Elaborates errors produced with the generic instance for records (#72)

As well as a new tutorial for the library (#62)

## [v6.0.2](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v6.0.2) - 2019-05-06

Fixed associativity of a type annotation in advance of new `purs` version

## [v6.0.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v6.0.1) - 2019-03-24

Fixes a typo in documentation (@JamieBallingall)

## [v6.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v6.0.0) - 2019-03-05

Updated dependencies

## [v5.1.3](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v5.1.3) - 2019-03-01

Uses the `elaborateFailure` function in `getFieldOptional'` for more descriptive error messages (@LucianU)

## [v5.1.2](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v5.1.2) - 2019-01-28

Fixed a typo in the `getFieldOptional` docs (@Jwhiles)

## [v5.1.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v5.1.1) - 2019-01-04

- Bumps minor dependencies (@thomashoneyman)
- Adds instances for `Set` (@bradediger)

## [v5.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v5.1.0) - 2018-11-24

Update combinators and operators to better reflect the usage pattern of Aeson, providing deprecation warnings for the old combinators (@davezuch)

## [v5.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v5.0.0) - 2018-11-09

Adds support for encoding and decoding record types (@elliotdavies)

## [v4.0.2](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v4.0.2) - 2018-06-27

- Adds better error messages (@crcornwell)
  - Tells you which index did not exist if you fail to access an array
  - Tells you which key did not exist if you fail to access an object

## [v4.0.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v4.0.1) - 2018-06-22

- Adds metadata including contributor guidelines
- Pushes latest release to Pursuit

## [v4.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v4.0.0) - 2018-06-04

Updates for 0.12

## [v3.3.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v3.3.0) - 2018-03-15

- Added `assocOptional` and `encodeOptional` functions and operators (@foresttoney, @kanterov)

## [v3.2.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v3.2.0) - 2017-07-10

- Added `(.?=)` operator for specifying a default value for an optional field (@cdepillabout)

## [v3.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v3.1.0) - 2017-05-24

- `getFieldOptional` and corresponding `(.??)` operator are now exported from `Data.Argonaut.Decode` (@cdepillabout)

## [v3.0.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v3.0.1) - 2017-04-24

- Reverted the change to the `Maybe` codec as it was not functioning correctly. The original behaviour has been restored.

## [v3.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v3.0.0) - 2017-04-08

- Updated for PureScript 0.11
- Encoding and decoding of `Maybe` has been updated to be able to accurately represent nested `Maybe`s. The `DecodeJson` instance is compatible with the old format, but `EncodeJson` produces a different structure now, so this may be a breaking change depending on your use case.

## [v2.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v2.1.0) - 2017-03-07

- Added codecs for `Void` (@natefaubion)

## [v2.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v2.0.0) - 2016-10-22

- Updated dependencies

## [v1.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v1.1.0) - 2016-08-05

- Added `(.??)` combinator for attempting to read fields that are optional (@passy)

## [v1.0.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v1.0.0) - 2016-06-11

Updates for the 1.0 core libraries.

## [v0.6.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.6.1) - 2016-01-15

- Fixed import warning

## [v0.6.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.6.0) - 2016-01-15

- The encoding of `Either` has been modified to be more explicit, allowing `Either` with the same type on either side to be decoded (@hdgarrood)

## [v0.5.2](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.5.2) - 2016-01-13

- Improved generic decoding errors (@hdgarrood)

## [v0.5.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.5.1) - 2015-12-22

- The `DecodeJson` instance for `Maybe` now treats `null` as `Nothing` to match the `EncodeJson` instance

## [v0.5.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.5.0) - 2015-11-20

- Updated for PureScript 0.7.6 and the updated generics (@zudov)

**Note**: this release _requires_ PureScript 0.7.6 or newer.

## [v0.4.1](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.4.1) - 2015-11-04

- Fix various warnings

## [v0.4.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.4.0) - 2015-11-04

Updated dependencies

## [v0.3.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.3.0) - 2015-08-25

## [v0.2.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.2.0) - 2015-08-19

- Updated dependencies for PureScript 0.7.3 (@zudov)

## [v0.1.0](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases/tag/v0.1.0) - 2015-07-13

- Initial release