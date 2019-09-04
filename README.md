# purescript-argonaut-codecs

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-codecs.svg)](https://github.com/purescript-contrib/purescript-argonaut-codecs/releases)
[![Build status](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-argonaut-codecs)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-codecs/badge)](http://pursuit.purescript.org/packages/purescript-argonaut-codecs/)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-lightgrey.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

`argonaut-codecs` provides codecs based on the `EncodeJson` and `DecodeJson` type classes, along with instances for common data types and combinators for encoding and decoding `Json` values.

## Installation

```sh
# with Spago
spago install argonaut-codecs

# with Bower
bower install purescript-argonaut-codecs
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-argonaut-codecs). 

You may also be interested in other libraries in the Argonaut ecosystem:

- [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core) -- defines the `Json` type, along with basic parsing, printing, and folding functions which operate on it
- [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals) -- defines prisms, traversals, and zippers for the `Json` type.

- [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic) -- supports generic encoding and decoding for any type with a `Generic` instance, which can be used to define instances of `EncodeJson` and `DecodeJson`.

Codecs based on type classes are not always the appropriate approach for encoding and decoding JSON in your application. See [purescript-codec-argonaut](https://github.com/garyb/purescript-codec-argonaut) for an alternative, more explicit approach, which includes a comparison to this library.

## Quick Start

Use `encodeJson` to encode PureScript data types as `Json` and `decodeJson` to decode `Json` into PureScript types, with helpful error messages if decoding fails.

```purs
type User =
  { name :: String 
  , age :: Maybe Int 
  }

-- We get encoding and decoding for free because of the `EncodeJson` instances 
-- for records, strings, integers, and `Maybe`, along with many other common
-- PureScript types.

userToJson :: User -> Json
userToJson = encodeJson

userFromJson :: Json -> Either String User
userFromJson = decodeJson
```

In a REPL we can see these functions in action:

```purs
> user = { name: "Tom", age: Just 25 }
> stringify (encodeJson user)
"{\"name\":\"Tom\",\"age\":25}"

> (decodeJson =<< jsonParser "{\"name\":\"Tom\",\"age\":25}") :: Either String User
Right { name: "Tom", age: Just 25 }

> (decodeJson =<< jsonParser "{\"name\":\"Tom\"}") :: Either String User
Left "JSON was missing expected field: age"
```

This library provides helpful combinators for writing `EncodeJson` and `DecodeJson` instances for your own data types. See [the tutorial](#Tutorial) for a detailed walkthrough of this library.

## Tutorial

Argonaut is a library for working with JSON in PureScript.

The core `Json` data type and basic parsing, printing and folding functions are defined in the `argonaut-core` library, but these functions are low-level.

This library, `argonaut-codecs`, provides type classes and combinators for convenient (and often automatic) encoding and decoding of `Json` for data types in your application, and includes instances for encoding and decoding most common PureScript types.

As a brief aside: this library works with `Json` values, not raw JSON strings.

- If you need to parse `Json` from a JSON string so that you can use `decodeJson`, then you should use the `jsonParser` function from `argonaut-core`.
- If you need to print `Json` as a valid JSON string (after using `encodeJson`, for example), then you should use the `stringify` function from `argonaut-core`.

#### Setup

We recommend following along with this tutorial in the repl. Start a repl and import these modules -- later, we'll define some types to encode and decode.

```
import Prelude

import Data.Argonaut
import Data.Maybe
import Data.Either
```

> Tip: you can place this snippet in a `.purs-repl` file so the imports are loaded automatically when you run `pulp repl` or `spago repl`.

### Automatic Encoding & Decoding 

The `EncodeJson` and `DecodeJson` type classes let you rely on instances for common data types to automatically encode and decode `Json`.

Let's explored automatic encoding and decoding using a type typical of PureScript applications as our example:

```purs
type User =
  { name :: String 
  , age :: Maybe Int
  , team :: Maybe String 
  }
```

> Tip: If you're following along in the repl, you can either define this type on one line or use `:paste` to input multiple lines followed by Ctrl+D to end the paste.

##### Automatic encoding with `EncodeJson` and `encodeJson`

Our `User` type is made up of several other types: `Record`, `Maybe`, `Int`, and `String`. Each of these types have instances for `EncodeJson`, which means that we can use the `encodeJson` function to transform them into `Json`:

```purs
encodeJson :: EncodeJson a => a -> Json
```

> Tip: There is no `Show` instance for `Json`. To print a `Json` value as a valid JSON string, use `stringify` -- it's the same as the [JavaScript  `stringify` method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify).

```purs
> user = { name: "Tom", age: Just 25, team: Just "Red Team" } :: User
> stringify (encodeJson user)
"{\"name\":\"Tom\",\"age\":25,\"team\":\"Red Team\"}"
```

##### Automatic decoding with `DecodeJson` and `decodeJson`

Every type within `User` has an instance for `DecodeJson`, which means we can use the `decodeJson` function to try to decode a `Json` value into our type:

```purs
decodeJson :: DecodeJson a => Json -> Either String a
```

> Tip: To parse a JSON string as a `Json` value, you can use the `jsonParser` function (which can fail). If you are sure you have valid JSON, then consider writing it in an FFI file and foreign importing it as `Json` as described in the [`argonaut-core` documentation](https://github.com/purescript-contrib/purescript-argonaut-core#introducing-json-values).

```purs
> userJsonString = """{ "name": "Tom", "age": 25, "team": null }"""
> decodedUser = decodeJson =<< jsonParser userJsonString

# there is no `Show` instance for `Json`, so we'll stringify 
# the decoded result so it can be displayed in the repl
> map stringify decodedUser
Right "{\"name\":\"Tom\",\"age\":25,\"team\":null}"
```

Decoding can fail if the `Json` doesn't match the shape expected by a `DecodeJson` instance; in that case, an error is returned instead of the decoded value.

```
> badUserJsonString = """{ "name": "Tom", "age": null }
> (decodeJson =<< jsonParser badUserJsonString) :: Either String User
Left "JSON was missing expected field: team"
```

### Writing New Instances

While instances of `EncodeJson` and `DecodeJson` exist for most common data types in the PureScript ecosystem, you will sometimes need to write your own. Common reasons to write your own instances include:

1. You have defined a new data type
2. You require `encodeJson` or `decodeJson` to behave differently, for a given type, than its existing `EncodeJson` or `DecodeJson` instance
3. You are using a data type which already exists, but does not have an `EncodeJson` or `DecodeJson` instance (typically because there are many reasonable ways to represent the data in JSON types, as is the case with dates).

It is also common to have a 'default' way to decode or encode a particular data type, but to write alternative decoding and encoding functions that can be used instead of the one supported by the type class.

Let's explore the combinators provided by `argonaut-codecs` for encoding and decoding `Json` by treating our `User` type as a new data type instead of just a synonym for a record, and turning the `team` field into a sum type instead of just a `String`.

> Remember that you can write multi-line definitions using by typing :paste in the repl, and then using Ctrl+D to exit when you're done.

```purs
newtype AppUser = AppUser 
  { name :: String
  , age :: Maybe Int
  , team :: Team
  }

data Team
  = RedTeam
  | BlueTeam
```

#### Encoding JSON

To encode JSON, you must decide on a way to represent your data using only primitive JSON types (strings, numbers, booleans, arrays, objects, or null). Since PureScript's string, number, boolean, and array types already have `EncodeJson` instances, your responsibility is to find a way to transform your data types to those more primitive types so they can be encoded.

Let's start with our `Team` type, which doesn't have an `EncodeJson` instance yet.  It can be represented in JSON by simple strings, so let's write a function to convert `Team` to a `String`:

```purs
teamToString :: Team -> String
teamToString = case _ of
  RedTeam -> "Red Team"
  BlueTeam -> "Blue Team"
```

We can now write an `EncodeJson` instance for our type. As a brief reminder, this is the type signature required by `encodeJson`:

```purs
encodeJson :: EncodeJson a => a -> Json
```

`String` already has an instance of `EncodeJson`, so all we need to do is convert our type to a string and then use `encodeJson` to encode the resulting string.

```purs
instance encodeJsonTeam :: EncodeJson Team where
  encodeJson team = encodeJson (teamToString team)
```

If your type can be converted easily to a `String`, `Number`, or `Boolean`, then its `EncodeJson` instance will most likely look like the one we've written for `Team`. 

Most reasonably complex data types are best represented as objects, however. We can use combinators from `Data.Argonaut.Encode.Combinators` to conveniently encode `Json` objects manually. You'll provide `String` keys and values which can be encoded to `Json`.

- Use `:=` (`assoc`) to encode a key/value pair where the key must exist; encoding the key `"team"` and value `Nothing ` will insert the key `"team"` with the value `null`.
- Use `~>` (`extend`) to provide more key/value pairs after using `:=`.
- Use `:=?` (`assocOptional`) to encode a key/value pair where the key _may_ exist; encoding the key `"age"` and value `Nothing` will not insert the `"age"` key.
- Use `~>?` (`extendOptional`) to provide more key/value pairs after using `:=?`.

Let's use these combinators to encode a `Json` object from our `AppUser` record.

```purs
instance encodeJsonAppUser :: EncodeJson AppUser where
  encodeJson (AppUser { name, age, team }) = 
    "name" := name       -- inserts "name": "Tom"
      ~> "age" :=? age   -- inserts "age": "25" (if Nothing, does not insert anything)
      ~>? "team" := team -- inserts "team": "Red Team"
      ~> jsonEmptyObject
```

To recap: manually encoding your data type involves a few steps:

1. Ensure that all types you are encoding have an `EncodeJson` instance or can be converted to another type which does.
2. Use `:=` or `:=?` to create a key/value pair in a JSON object
3. Use `~>` or `~>?` to chain together multiple key/value pairs.

Ultimately, this will produce an `Object Json` which can be serialized to a JSON string or manipulated.

#### Decoding JSON

Decoding PureScript types from `Json` is similar to encoding them. You'll once again need a mapping from your data type to its representation in primitive JSON types. Booleans, strings, numbers, and arrays are covered by existing `DecodeJson` instances, so if you can convert from any of those types to your PureScript type then you can use that conversion to write a `DecodeJson` instance for your type.

Let's begin once again with our `Team` type, which can be represented as a string in JSON and does not have a `DecodeJson` instance yet. We'll start by writing a function which tries to produce a `Team` from a `String`:

```purs
teamFromString :: String -> Maybe Team
teamFromString = case _ of
  "Red Team" -> Just RedTeam
  "Blue Team" -> Just BlueTeam
  _ -> Nothing
```

We can use this function to write a `DecodeJson` instance for our type. As a quick reminder, this is the type signature required by `decodeJson`:

```purs
decodeJson :: DecodeJson a => Json -> Either String a
```

Let's write the instance using `note` from `purescript-either`:

```purs
instance decodeJsonTeam :: DecodeJson Team where
  decodeJson string = do
    let decodeError = "Could not decode Team from " <> string
    decodeJson (note decodeError teamFromString string)
```

If your type can be represented easily with a `String`, `Number`, `Boolean`, or array of one of these types, then its `DecodeJson` will most likely look similar to this one.

However, quite often your data type will require representation as an object. This library provides combinators in `Data.Argonaut.Decode.Combinators` which are useful for decoding objects into PureScript types by looking up keys in the object and decoding them according to their `DecodeJson` instances.

- Use `.:` (`getField`) to decode a field where the value must exist
- Use `.:?` (`getFieldOptional'`) to decode a field where the key may not exist, or the value may be `null`
- Use `.:!` (`getFieldOptional`) to decode a field where the key may not exist. If the key exists, the value will be decoded -- even if it is `null`. This is rare; most of the time you want `.:?` instead.
- Use `.!=` (`defaultField`) to provide a default value for a field which may not exist. If decoding fails, you'll still get an error; if decoding succeeds with a value of type  `Maybe a`, then this default value will handle the `Nothing` case.

Let's use these combinators to decode a `Json` object into our `AppUser` record. 

The `decodeJson` function returns an `Either String a` value; `Either` is a monad, which means we can use convenient `do` syntax to write our decoder. If a step in decoding succeeds, then its result is passed to the next step. If any step in decoding fails, the entire computation will abort with the error it encountered.

```purs
instance decodeJsonAppUser :: DecodeJson AppUser where
  decodeJson json = do
    obj <- decodeJson json             -- decode `Json` to `Object Json`
    name <- obj .: "name"              -- decode the "name" key to a `String`
    age <- obj .:? "age"               -- decode the "age" key to a `Maybe Int`
    team <- obj .:? "team" .!= RedTeam -- decode "team" to `Team`, defaulting to `RedTeam`
                                       -- if the field is missing or `null`
    pure $ AppUser { name, age, team }
```

To recap: manually decoding your data type involves a few steps:

1. Ensure that all types you are decoding have a `DecodeJson` instance

2. Use `.:` to decode object fields which must exist, `.:?` for fields which may exist or may be null, and `.!=` to provide a default value for fields which may exist and are being decoded into a type which must exist.

3. It's common to use the `Either` monad for convenience when writing decoders. Any failed decoding step will abort the entire computation with that error. See [Solving Common Problems](#solving-common-problems) for alternative approaches to decoding.

#### Deriving Instances

There are two ways to derive instances of `EncodeJson` and `DecodeJson` for new types.

##### Newtype Deriving

We intentionally introduced a newtype around a record, `AppUser`, so that we could hand-write type class instances for it. What if we'd needed the newtype for another reason, and we planned on using the same encoding and decoding as the underlying type's instances provide?

In that case, we can use newtype deriving to get `EncodeJson` and `DecodeJson` for our newtype for free:

```purs
newtype AppUser = AppUser { name :: String, age :: Maybe Int, team :: Team }

instance newtypeAppUser :: Newtype AppUser _

derive newtype instance encodeJsonAppUser :: EncodeJson AppUser
derive newtype instance decodeJsonAppUser :: DecodeJson AppUser
```

##### Generics

If your data type has an instance of `Generic`, then you can use [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic) to leverage `gEncodeJson` and `gDecodeJson` to write your instances:

```purs
data Team = RedTeam | BlueTeam

derive instance genericTeam :: Generic Team _

instance encodeJsonTeam :: EncodeJson Team where
  encodeJson = gEncodeJson

instance decodeJsonTeam :: DecodeJson Team where
  decodeJson = gDecodeJson
```

### Solving Common Problems

#### Handling Multiple JSON Representations

Sometimes a data type in your application can be represented in multiple formats. For example, consider a `User` type like this:

```purs
newtype User = User
  { uuid :: String
  , name :: String
  }
```

In previous versions of your API the `uuid` field has been named `uid` and `id`. Unfortunately, you receive data from all three versions, so you need to accommodate each. You only want one canonical type in your application, though: the `User` type above.

There are several ways to handle the case in which a data type has multiple JSON representations.

##### 1. Use `Alternative` to provide fallback decoders

The first option is to use the `Alternative` type class and its `<|>` operator to provide multiple ways to decode a particular field in an object. For example:

```purs
instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    uuid <- obj .: "uuid" <|> obj .: "uid" <|> obj .: "id"
    pure $ User { name, uuid }
```

You may sometimes need to do additional processing so that `uuid` always ends up being decoded to the correct type. For example, if in a previous API version the `id` field was actually an object with a `value` field containing the id, then you could provide a two-step decoder for that case.

```purs
instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    ...
    uuid <- obj .: "uuid" <|> obj .: "uid" <|> ((_ .: "value") =<< obj .: "id")
```

##### 2. Write multiple `encodeJson ` or `decodeJson` functions

Another option is to have a default representation for the type implemented as the type class instance, but alternative `decodeJson` and `encodeJson` functions which can be used directly. For example, consider the case in which our `User` data can be sent to multiple sources. One source requires the data to be formatted as an object, and another requires it to be formatted as a two-element array. 

In this case, our type class instance can use the default object encoding, and we can supply a separate `encodeJsonAsArray` function for use when required.

```purs
-- our default object encoding
derive newtype instance encodeJsonUser :: EncodeJson User

encodeUserAsArray :: User -> Json
encodeUserAsArray user = encodeJson [ user.uuid, user.name ]
```

#### Decoding With More Arguments than `Json`

You may occasionally be unable to write `EncodeJson` or `DecodeJson` instances for a data type because it requires more information than just `Json` as its argument. For instance, consider this pair of types:

```purs
data Author 
  = Following String    -- you are subscribed to this author
  | NotFollowing String -- you aren't subscribed to this author
  | You                 -- you are the author

type BlogPost =
  { title :: String 
  , author :: Author
  }
```

Our API tells us the author of the blog post as a string and whether we follow them as a boolean. This admits more cases than are actually possible -- you can't follow yourself, for example -- so we are more precise and model an `Author` as a sum type.

When our application is running we know who the currently-authenticated user is, and we can use that information to determine the `Author` type. That means we can't decode an `Author` from `Json` alone -- we need more information.

In these cases, unfortunately, you can't write an instance of `DecodeJson` for the data type. You can, however, write `decodeJsonAuthor` and use it without the type class. For instance:

```purs
decodeJsonAuthor :: Maybe Username -> Json -> Either String Author
decodeJsonAuthor maybeUsername json = do
  obj <- decodeJson json
  author <- obj .: "author"
  following <- obj .: "following"
  pure $ case maybeUsername of
    -- user is logged in and is the author
    Just (Username username) | author == username -> You
    -- user is not the author, or no one is logged in, so use the `following` flag
    otherwise -> author # if following then Following else NotFollowing
    
decodeJsonBlogPost :: Maybe Username -> Json -> Either String BlogPost
decodeJsonBlogPost username json = do
  obj <- decodeJson json
  title <- obj .: "title"
  author <- decodeJsonAuthor username =<< obj .: "author"
  pure { title, author }
```

#### Writing Instances For Types You Don't Own

While not an issue specific to `argonaut-codecs`, you may sometimes wish to write an `EncodeJson` or a `DecodeJson` instance for a data type you did not define -- for instance, the `PreciseDateTime` type from `purescript-precise-datetime`. This type has no instances because there are many ways you might wish to represent it in JSON.

If you want to use an application-specific encoding for this type then you will need to define a newtype wrapper for it and define instances for that new type instead.

```purs
module App.Data.PreciseDateTime where

import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

newtype PreciseDateTime = PreciseDateTime PDT.PreciseDateTime

instance decodeJsonPreciseDateTime :: DecodeJson PreciseDateTime where
  decodeJson json = fromString =<< decodeJson json
    where
    fromString :: String -> Either String PreciseDateTime
    fromString = 
      map PreciseDateTime
        <<< note "Could not parse RFC3339 string" 
        <<< PDT.fromRFC3339String 
        <<< RFC3339String
```

You can now use the wrapped `PreciseDateTime` type in your application and the instance will be used by the `DecodeJson` type class.

#### Accumulating Errors Instead of Short-Circuiting

You may sometimes want to _accumulate_ errors, rather than short-circuit at the first failure. The `V` type from `purescript-validation` is similar to `Either`, but it allows you to accumulate errors into a semigroup or semiring instead of stopping when the first failure occurs. You can define decoders which work in `V` and then convert them back to `Either` at the end.

For example, let's say we have a `User` type which occasionally gets bad input, and we want to see _all_ errors in the input rather than one at a time. This is how we might write a decoding function for the type:

```purs
newtype User = User
  { name :: String 
  , age :: Maybe Int
  , location :: String
  }

derive instance newtypeUser :: Newtype User _
derive newtype instance showUser :: Show User

decodeUser :: Json -> Either (Array String) User 
decodeUser json = do
  obj <- decodeJson json
  name <- obj .: "name"
  age <- obj .: "age"
  location <- obj .: "location"
  pure $ User { name, age, location }
```

Running this in the REPL with bad input, we only see the first error:

```purs
> decodeUser =<< jsonParser "{}"
Left "Expected field \"name\""
```

However, by collecting results into `V` instead of into `Either` we will accumulate all errors. We can even make it a little nicer by writing a new operator, `.:|`, which works in `V`:

```purs
-- a replacement for `decodeJson`
decodeJsonV :: forall a. DecodeJson a => Json -> V (Array String) a
decodeJsonV = either (invalid <<< pure) pure <<< decodeJson

-- a replacement for `getField`
getFieldV :: forall a. DecodeJson a => Object Json -> String -> V (Array String) a
getFieldV object key = 
  either (invalid <<< pure) pure (object .: key)

-- a replacement for .:
infix 7 getFieldV as .:|
```

With this new operator and applicative-do we can recreate our original decoder, except with accumulating errors this time:

```purs
decodeUser :: Json -> Either (Array String) User 
decodeUser json = do
  user <- toEither $ V.andThen (decodeJsonV json) \obj -> ado
    name <- obj .:| "name"
    age <- obj .:| "age"
    location <- obj .:| "location"
    in { name, age, location }
  pure (User user)
```

This decoder will now print all errors:

```purs
> import Data.Bifunctor (lmap)
> decodeUser =<< lmap pure (jsonParser "{}")
Left 
  [ "Expected field \"name\""
  , "Expected field \"age\""
  , "Expected field \"location\""
  ]
```

## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/.github/contributing.md) to get started and see helpful related resources.
