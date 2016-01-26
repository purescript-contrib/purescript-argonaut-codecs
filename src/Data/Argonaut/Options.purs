module Data.Argonaut.Options where

import Prelude
import Data.Foldable (all)
import Data.String (lastIndexOf, drop)
import Data.Generic (DataConstructor())
import Data.Array (null, length)
import Data.Maybe (Maybe(..))


type Options = {
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
argonautOptions = {
  constructorTagModifier : id
, allNullaryToStringTag  : false
, sumEncoding            : argonautSumEncoding
, flattenContentsArray   : false
, unwrapUnaryRecords     : false
}

argonautSumEncoding :: SumEncoding
argonautSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "values"
}

-- | Options for aeson compatible encoding/decoding.
aesonOptions :: Options
aesonOptions = {
  constructorTagModifier : stripModulePath
, allNullaryToStringTag  : true
, sumEncoding            : aesonSumEncoding
, flattenContentsArray   : true
, unwrapUnaryRecords     : false
}

aesonSumEncoding :: SumEncoding
aesonSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "contents"
}



allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary = all (null <<< _.sigValues)

isUnaryRecord :: Array DataConstructor -> Boolean
isUnaryRecord constrSigns = length constrSigns == 1 -- Only one constructor
                            && all ((== 1) <<< length <<< _.sigValues)  constrSigns -- Only one parameter

stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr
