module Data.Argonaut.Decode.Combinators
  ( getField
  , getFieldDeprecated
  , getFieldOptional
  , getFieldOptionalDeprecated
  , getFieldOptional'
  , defaultField
  , defaultFieldDeprecated
  , (.:)
  , (.?)
  , (.:!)
  , (.:?)
  , (.??)
  , (.!=)
  , (.?=)
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object as FO
import Prim.TypeError (class Warn, Text)

-- | Attempt to get the value for a given key in an `Object Json`. Example:
-- |
-- | ```purs
-- | newtype User = User { name :: String, age :: Maybe Int, location :: String }
-- |
-- | instance decodeJsonUser :: DecodeJson User where
-- |   decodeJson json = do
-- |     obj <- decodeJson json
-- |     name <- obj .: "name"
-- |     age <- obj .: "age"
-- |     location <- obj .: "location"
-- |     pure $ User { name, age, location }
-- | ```
getField :: forall a. DecodeJson a => FO.Object Json -> String -> Either String a
getField o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decodeJson)
    (FO.lookup s o)

infix 7 getField as .:

getFieldDeprecated
  :: forall a. Warn (Text "`.?` is deprecated. Use `.:` instead")
  => DecodeJson a
  => FO.Object Json
  -> String
  -> Either String a
getFieldDeprecated = getField

infix 7 getFieldDeprecated as .?

-- | Attempt to get the value for a given key on an `Object Json`. Deprecated: 
-- | use `.:` (`getField`) instead.
getFieldOptional' 
  :: forall a
   . Warn (Text "`.:?` is deprecated because its behavior is identical to the `Maybe` instance for `.:`. Use `.:` instead.")
  => DecodeJson a 
  => FO.Object Json 
  -> String 
  -> Either String (Maybe a)
getFieldOptional' = getField

infix 7 getFieldOptional' as .:?

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to 
-- | the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into 
-- | your desired type. If you would like to treat `null` values the same as 
-- | absent values, use `getField'` (`.:`) instead.
getFieldOptional :: forall a. DecodeJson a => FO.Object Json -> String -> Either String (Maybe a)
getFieldOptional o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
  decode json = Just <$> (elaborateFailure s <<< decodeJson) json

infix 7 getFieldOptional as .:!

-- | Attempt to get the value for a given key on an `Object Json`. Deprecated: 
-- | use `.:` (`getField`) instead.
getFieldOptionalDeprecated
  :: forall a. Warn (Text "`.??` is deprecated. Use `.:` instead.")
  => DecodeJson a
  => FO.Object Json
  -> String
  -> Either String (Maybe a)
getFieldOptionalDeprecated = getField

infix 7 getFieldOptionalDeprecated as .??

-- | Helper for use in combination with `.:!` to provide default values for 
-- | fields which may exist in `Object Json` but must exist in the decoded type.
-- |
-- | Note: If you are using `.:`, this combinator is unnecessary. Replace 
-- | `x .: "key" .!= default` with `x .: "key" <|> pure default`, where `<|>` is 
-- | from `Control.Alternative`.
-- |
-- | Example usage:
-- | ```purs
-- | newtype MyType = MyType { foo :: String, baz :: Boolean }
-- |
-- | instance decodeJsonMyType :: DecodeJson MyType where
-- |   decodeJson json = do
-- |     x <- decodeJson json
-- |     foo <- x .: "foo"           -- field must exist in `Json`
-- |     baz <- x .: "baz" .!= false -- field may exist in `Json`; if not, defaulted to `false`
-- |     pure $ MyType { foo, baz }
-- | ```
defaultField :: forall a. Either String (Maybe a) -> a -> Either String a
defaultField parser default = fromMaybe default <$> parser

infix 6 defaultField as .!=

defaultFieldDeprecated
  :: forall a
   . Warn (Text """`.?=` is deprecated. Replace `x .? "key" .?= default` with `x .: "key" <|> pure default`.""")
  => Either String (Maybe a) 
  -> a 
  -> Either String a
defaultFieldDeprecated = defaultField

infix 6 defaultFieldDeprecated as .?=

elaborateFailure :: ∀ a. String -> Either String a -> Either String a
elaborateFailure s e =
  lmap msg e
  where
  msg m = "Failed to decode key '" <> s <> "': " <> m
