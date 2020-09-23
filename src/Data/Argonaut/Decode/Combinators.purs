module Data.Argonaut.Decode.Combinators
  ( getField
  , getFieldOptional
  , getFieldOptional'
  , defaultField
  , (.:)
  , (.:!)
  , (.:?)
  , (.!=)
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Foreign.Object as FO
import Data.Argonaut.Decode.Decoders as Decoders

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | Use this accessor if the key and value *must* be present in your object.
-- | If the key and value are optional, use `getFieldOptional'` (`.:?`) instead.
getField :: forall a. DecodeJson a => FO.Object Json -> String -> Either JsonDecodeError a
getField = Decoders.getField decodeJson

infix 7 getField as .:

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getField` (`.:`) instead.
getFieldOptional' :: forall a. DecodeJson a => FO.Object Json -> String -> Either JsonDecodeError (Maybe a)
getFieldOptional' = Decoders.getFieldOptional' decodeJson

infix 7 getFieldOptional' as .:?

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `getFieldOptional'` (`.:?`) instead.
getFieldOptional :: forall a. DecodeJson a => FO.Object Json -> String -> Either JsonDecodeError (Maybe a)
getFieldOptional = Decoders.getFieldOptional decodeJson

infix 7 getFieldOptional as .:!

-- | Helper for use in combination with `.:?` to provide default values for optional
-- | `Object Json` fields.
-- |
-- | Example usage:
-- | ```purs
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
defaultField :: forall a. Either JsonDecodeError (Maybe a) -> a -> Either JsonDecodeError a
defaultField parser default = fromMaybe default <$> parser

infix 6 defaultField as .!=
