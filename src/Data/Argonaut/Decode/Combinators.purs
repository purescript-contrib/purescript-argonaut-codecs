module Data.Argonaut.Decode.Combinators where

import Prelude

import Data.Argonaut.Core (JObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.StrMap as SM

getField :: forall a. DecodeJson a => JObject -> String -> Either String a
getField o s =
  maybe
    (Left $ "Expected field " <> show s)
    decodeJson
    (SM.lookup s o)

infix 7 getField as .?
