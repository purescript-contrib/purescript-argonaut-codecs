module Data.Argonaut.Decode.Parser where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)

-- | Attempt to parse a string as `Json`, failing with a typed error if the
-- | JSON string is malformed.
parseJson :: String -> Either JsonDecodeError Json
parseJson = lmap (\_ -> TypeMismatch "JSON") <<< jsonParser
