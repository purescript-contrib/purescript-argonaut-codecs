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

import Data.Argonaut.Core (Json, caseJsonObject, fromObject, jsonSingletonObject)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

-- | Creates a `Tuple String Json` entry, representing a key/value pair for an object.
infix 7 assoc as :=

-- | The named implementation of the `(:=)` operator.
assoc :: forall a. EncodeJson a => String -> a -> Tuple String Json
assoc k = Tuple k <<< encodeJson

-- | Creates an optional `Tuple String Json` entry, representing an optional key/value pair for an object.
infix 7 assocOptional as :=?

-- | The named implementation of the `(:=?)` operator.
assocOptional
  :: forall a
   . EncodeJson a
  => String
  -> Maybe a
  -> Maybe (Tuple String Json)
assocOptional k = (<$>) (((:=) k) <<< encodeJson)

-- | Extends a Json object with a `Tuple String Json` property.
infixr 6 extend as ~>

-- | The named implementation of the `(~>)` operator.
extend :: forall a. EncodeJson a => Tuple String Json -> a -> Json
extend (Tuple k v) =
  caseJsonObject
    (jsonSingletonObject k v)
    (FO.insert k v >>> fromObject)
    <<< encodeJson

-- | Optionally extends a Json object with an optional `Tuple String Json` property.
infixr 6 extendOptional as ~>?

-- | The named implementation of the `(~>?)` operator.
extendOptional :: forall a. EncodeJson a => Maybe (Tuple String Json) -> a -> Json
extendOptional (Just kv) = (~>) kv
extendOptional Nothing = encodeJson
