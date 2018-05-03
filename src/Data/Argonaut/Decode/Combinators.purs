module Data.Argonaut.Decode.Combinators where

import Prelude

import Data.Argonaut.Core (JObject, Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap as SM

getField :: forall a. DecodeJson a => JObject -> String -> Either String a
getField =
  getField' decodeJson

getField' :: forall a. (Json -> Either String a) -> JObject -> String -> Either String a
getField' f o s =
  maybe
    (Left $ "Expected field " <> show s)
    f
    (SM.lookup s o)

infix 7 getField as .?

getFieldOptional :: forall a. DecodeJson a => JObject -> String -> Either String (Maybe a)
getFieldOptional o s =
  maybe
    (pure Nothing)
    decode
    (SM.lookup s o)
  where
    decode json = Just <$> decodeJson json

infix 7 getFieldOptional as .??

defaultField :: forall a. Either String (Maybe a) -> a -> Either String a
defaultField parser default = fromMaybe default <$> parser

infix 6 defaultField as .?=
