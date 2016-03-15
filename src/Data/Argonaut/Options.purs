module Data.Argonaut.Options where

import Prelude
import Data.Argonaut.Core (Json())
import Data.Foldable (all)
import Data.String (lastIndexOf, drop)
import Data.Generic (DataConstructor())
import Data.Array (null, length)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor(), toSignature)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))


newtype Options = Options { -- newtype necessary to avoid: https://github.com/purescript/purescript/wiki/Error-Code-CycleInTypeSynonym
  -- | Modify the tag, e.g. strip module path with: `stripModulePath`
  constructorTagModifier  :: String -> String
  -- | If all constructors of a sum type are nullary, just serialize the constructor name as a string.
, allNullaryToStringTag   :: Boolean
  -- | Options on how to do encoding of sum types.
, sumEncoding             :: SumEncoding
  -- | If a constructor has exactly one field, do not serialize as array.
, flattenContentsArray    :: Boolean -- Flatten array to simple value, if constructor only takes a single value
  -- | You need a newtype wrapper encoding/decoding of records, set this
  -- | to true if you want the plain Javascript object without a wrapping tagged object.
, unwrapUnaryRecords :: Boolean
-- | You can choose to encode some data types differently than the generic default.
-- | Just return Nothing if you want to relay to generic encoding.
, userEncoding :: Options -> GenericSignature -> GenericSpine -> Maybe Json
-- | You can choose to decode some data types differently than the generic default.
-- | Just return Nothing, to relay to generic decoding.
, userDecoding :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
}

data SumEncoding =
  -- | Serialize as tagged object.
  -- | The Javascript object will have a tag field, with the
  -- | `constructorTagModifier constructorName` name as contents
  -- | and a contents field, which contains an array with the constructor
  -- | parameters.
  TaggedObject {
    tagFieldName :: String
  , contentsFieldName :: String
  }

-- | Default for straight forward argonaut encoding.
argonautOptions :: Options
argonautOptions = Options {
  constructorTagModifier : id
, allNullaryToStringTag  : false
, sumEncoding            : argonautSumEncoding
, flattenContentsArray   : false
, unwrapUnaryRecords     : false
, userEncoding           : dummyUserEncoding
, userDecoding           : dummyUserDecoding
}

argonautSumEncoding :: SumEncoding
argonautSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "values"
}

dummyUserEncoding :: Options -> GenericSignature -> GenericSpine -> Maybe Json
dummyUserEncoding _ _ _ = Nothing

dummyUserDecoding :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
dummyUserDecoding _ _ _ = Nothing

allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary = all (null <<< _.sigValues)

isUnaryRecord :: Array DataConstructor -> Boolean
isUnaryRecord constrSigns = length constrSigns == 1 -- Only one constructor
                            && all ((== 1) <<< length <<< _.sigValues)  constrSigns -- Only one parameter

stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr
