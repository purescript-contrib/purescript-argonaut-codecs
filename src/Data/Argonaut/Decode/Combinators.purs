module Data.Argonaut.Decode.Combinators where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object as FO

getField :: forall a. DecodeJson a => FO.Object Json -> String -> Either String a
getField o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decodeJson)
    (FO.lookup s o)

infix 7 getField as .?

getFieldOptional :: forall a. DecodeJson a => FO.Object Json -> String -> Either String (Maybe a)
getFieldOptional o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< decodeJson) json

infix 7 getFieldOptional as .??

defaultField :: forall a. Either String (Maybe a) -> a -> Either String a
defaultField parser default = fromMaybe default <$> parser

infix 6 defaultField as .?=

elaborateFailure :: ∀ a. String -> Either String a -> Either String a
elaborateFailure s e =
  lmap msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m
