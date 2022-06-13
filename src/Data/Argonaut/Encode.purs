module Data.Argonaut.Encode
  ( module Data.Argonaut.Encode.Class
  , module Data.Argonaut.Encode.Combinators
  , toJson
  )
  where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators (assoc, assocOptional, extend, extendOptional, (:=), (:=?), (~>), (~>?))

-- | Encode and stringify a type in one step.
toJson :: forall t. EncodeJson t => t -> String
toJson = encodeJson >>> stringify
