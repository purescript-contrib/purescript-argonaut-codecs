-- | Provides operators for a DSL to construct `Json` values:
-- |
-- | ```purs
-- | myJson =
-- |  "key1" := value1
-- |    ~> "key2" :=? value2
-- |    ~>? "key3" := value3
-- |    ~> jsonEmptyObject
-- | ```
module Data.Argonaut.Encode.Combinators where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Argonaut.Encode.Encoders as Encoders

-- | Creates a `Tuple String Json` entry, representing a key/value pair for
-- | an object.
infix 7 assoc as :=

-- | The named Encoders of the `(:=)` operator.
assoc :: forall a. EncodeJson a => String -> a -> Tuple String Json
assoc = Encoders.assoc encodeJson

-- | Creates an optional `Tuple String Json` entry, representing an optional
-- | key/value pair for an object.
infix 7 assocOptional as :=?

-- | The named Encoders of the `(:=?)` operator.
assocOptional
  :: forall a
   . EncodeJson a
  => String
  -> Maybe a
  -> Maybe (Tuple String Json)
assocOptional = Encoders.assocOptional encodeJson

-- | Extends a Json object with a `Tuple String Json` property.
infixr 6 extend as ~>

-- | The named Encoders of the `(~>)` operator.
extend :: forall a. EncodeJson a => Tuple String Json -> a -> Json
extend = Encoders.extend encodeJson

-- | Optionally extends a Json object with an optional `Tuple String Json` property.
infixr 6 extendOptional as ~>?

-- | The named Encoders of the `(~>?)` operator.
extendOptional :: forall a. EncodeJson a => Maybe (Tuple String Json) -> a -> Json
extendOptional = Encoders.extendOptional encodeJson
