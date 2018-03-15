-- | Provides operators for a DSL to construct `Json` values:
-- |
-- | ``` purescript
-- | myJson
-- |  = "key1" := value1
-- |  ~> "key2" :=? value2
-- | ~>? "key3" := value3
-- |  ~> jsonEmptyOibject
-- | ```
module Data.Argonaut.Encode.Combinators where

import Prelude

import Data.Argonaut.Core (Json, JAssoc, foldJsonObject, fromObject, jsonSingletonObject)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

-- | Creates a `JAssoc` entry, representing a key/value pair for an object.
infix 7 assoc as :=

-- | The named implementation of the `(:=)` operator.
assoc :: forall a. EncodeJson a => String -> a -> JAssoc
assoc k = Tuple k <<< encodeJson

-- | Creates an optional `JAssoc` entry, representing an optional key/value pair for an object.
infix 7 assocOptional as :=?

-- | The named implementation of the `(:=?)` operator.
assocOptional :: forall a. EncodeJson a => String -> Maybe a -> Maybe JAssoc
assocOptional k = (<$>) (((:=) k) <<< encodeJson)

-- | Extends a Json object with a `JAssoc` property.
infixr 6 extend as ~>

-- | The named implementation of the `(~>)` operator.
extend :: forall a. EncodeJson a => JAssoc -> a -> Json
extend (Tuple k v) =
  foldJsonObject
    (jsonSingletonObject k v)
    (SM.insert k v >>> fromObject)
    <<< encodeJson

-- | Optionally extends a Json object with an optional `JAssoc` property.
infixr 6 extendOptional as ~>?

-- | The named implementation of the `(~>?)` operator.
extendOptional :: forall a. EncodeJson a => Maybe JAssoc -> a -> Json
extendOptional (Just kv) = (~>) kv
extendOptional Nothing = encodeJson