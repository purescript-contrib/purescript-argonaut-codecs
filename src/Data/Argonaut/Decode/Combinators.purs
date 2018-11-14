module Data.Argonaut.Decode.Combinators
  ( parseField
  , parseFieldOptional
  , parseFieldOptional'
  , defaultField
  , (.:)
  , (.:!)
  , (.:?)
  , (.!=)
  ) where

import Prelude

import Data.Argonaut.Core (Json, isNull)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object as FO

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | Use this accessor if the key and value *must* be present in your object.
-- | If the key and value are optional, use `parseFieldOptional'` (`.:?`) instead.
parseField :: forall a. DecodeJson a => FO.Object Json -> String -> Either String a
parseField o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decodeJson)
    (FO.lookup s o)

infix 7 parseField as .:

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `parseField` (`.:`) instead.
parseFieldOptional' :: forall a. DecodeJson a => FO.Object Json -> String -> Either String (Maybe a)
parseFieldOptional' o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json =
      if isNull json
        then pure Nothing
        else Just <$> decodeJson json

infix 7 parseFieldOptional' as .:?

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `parseFieldOptional` (`.:?`) instead.
parseFieldOptional :: forall a. DecodeJson a => FO.Object Json -> String -> Either String (Maybe a)
parseFieldOptional o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< decodeJson) json

infix 7 parseFieldOptional as .:!

-- | Helper for use in combination with `.:?` to provide default values for optional
-- | `Object Json` fields.
-- |
-- | Example usage:
-- | ```purescript
-- | newtype MyType = MyType
-- |   { foo :: String
-- |   , bar :: Maybe Int
-- |   , baz :: Boolean
-- |   }
-- |
-- | instance decodeJsonMyType :: DecodeJson MyType where
-- |   decodeJson json = do
-- |     x <- decodeJson json
-- |     foo <- x .: "foo" -- mandatory field
-- |     bar <- x .:? "bar" -- optional field
-- |     baz <- x .:? "baz" .!= false -- optional field with default value of `false`
-- |     pure $ MyType { foo, bar, baz }
-- | ```
defaultField :: forall a. Either String (Maybe a) -> a -> Either String a
defaultField parser default = fromMaybe default <$> parser

infix 6 defaultField as .!=

elaborateFailure :: ∀ a. String -> Either String a -> Either String a
elaborateFailure s e =
  lmap msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m
