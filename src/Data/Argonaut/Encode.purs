module Data.Argonaut.Encode
  ( module Data.Argonaut.Encode.Class
  , module Data.Argonaut.Encode.Combinators
  , toJsonString
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators (assoc, assocOptional, extend, extendOptional, (:=), (:=?), (~>), (~>?))

-- | Encode and stringify a type in one step.
toJsonString :: forall t. EncodeJson t => t -> String
toJsonString = encodeJson >>> stringify
