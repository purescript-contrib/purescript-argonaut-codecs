module Data.Argonaut.Options where

import Prelude
import Data.Foldable (foldr)
import Data.String (lastIndexOf, drop)
import Data.Generic (DataConstructor())
import Data.Array (null)
import Data.Maybe (Maybe(..))


type Options = {
  constructorTagModifier  :: String -> String
, allNullaryToStringTag   :: Boolean
, sumEncoding             :: SumEncoding
, flattenContentsArray    :: Boolean -- Flatten array to simple value, if constructor only takes a single value
}

data SumEncoding = TaggedObject {
  tagFieldName :: String
, contentsFieldName :: String
}

argonautOptions :: Options
argonautOptions = {
  constructorTagModifier : id
, allNullaryToStringTag  : false
, sumEncoding            : argonautSumEncoding
, flattenContentsArray   : false
}

argonautSumEncoding :: SumEncoding
argonautSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "values"
}

aesonOptions :: Options
aesonOptions = {
  constructorTagModifier : stripModulePath
, allNullaryToStringTag  : true
, sumEncoding            : aesonSumEncoding
, flattenContentsArray   : true
}

aesonSumEncoding :: SumEncoding
aesonSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "contents"
}



allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary constrSigns = foldr (&&) true <<< map (\c -> null c.sigValues) $ constrSigns

stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr
