module Data.Argonaut.GJson where

import Prelude

import Control.Comonad (class Extend, class Comonad)

import Data.Argonaut.Decode.Class (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, gEncodeJson)
import Data.Generic (class Generic)

-- | A newtype wrapper to direct instance selection to use `Generic`-based
-- | `Json` encoding and decoding for a given type.
newtype GJson a = GJson a

unGJson :: forall a. GJson a -> a
unGJson (GJson a) = a

instance functorGJson :: Functor GJson where
  map f (GJson a) = GJson (f a)

instance extendGJson :: Extend GJson where
  extend f j = GJson (f j)

instance comonadGJson :: Comonad GJson where
  extract (GJson a) = a

instance encodeJsonGJson :: Generic a => EncodeJson (GJson a) where
  encodeJson (GJson x) = gEncodeJson x

instance decodeJsonGJson :: Generic a => DecodeJson (GJson a) where
  decodeJson = map GJson <<< gDecodeJson
