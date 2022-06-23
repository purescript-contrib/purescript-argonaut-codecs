module Data.Argonaut.Decode
  ( fromJsonString
  , module Data.Argonaut.Decode.Class
  , module Data.Argonaut.Decode.Combinators
  , module Data.Argonaut.Decode.Error
  , module Data.Argonaut.Decode.Parser
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional, getFieldOptional', defaultField, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Either (Either)

-- | Parse and decode a json in one step.
fromJsonString :: forall json. DecodeJson json => String -> Either JsonDecodeError json
fromJsonString = parseJson >=> decodeJson
