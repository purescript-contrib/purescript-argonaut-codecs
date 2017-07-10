module Data.Argonaut.Decode
  ( module Data.Argonaut.Decode.Class
  , module Data.Argonaut.Decode.Combinators
  ) where

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators (getField, (.?), getFieldOptional, (.??), defaultField, (.?=))
