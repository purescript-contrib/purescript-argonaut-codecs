-- Haskell Aeson compatible encoding/decoding:

module Data.Argonaut.Aeson
  ( gAesonEncodeJson
  , gAesonDecodeJson
  , aesonOptions
  , aesonUserEncoding
  , aesonUserDecoding
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject, JArray, jsonNull)
import Data.Argonaut.Options
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor(), toSignature)
import Data.Int (toNumber)
import Data.List (List(..), fromList)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromChar)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (uncurry)
import Data.Array (length, concatMap, filter, zip, zipWith)
import qualified Data.Array.Unsafe as Unsafe
import Partial.Unsafe (unsafeCrashWith)


-- | Options for aeson compatible encoding/decoding.
aesonOptions :: Options
aesonOptions = Options {
  constructorTagModifier : stripModulePath
, allNullaryToStringTag  : true
, sumEncoding            : aesonSumEncoding
, flattenContentsArray   : true
, unwrapUnaryRecords     : false
, userEncoding           : aesonUserEncoding
, userDecoding           : aesonUserDecoding
}

aesonSumEncoding :: SumEncoding
aesonSumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "contents"
}

-- | Encode `Json` representation of a value which has a `Generic` type
-- | with Aeson options. The encoded data will be compatible with Haskell Aeson,
-- | if Aeson default options are used.
gAesonEncodeJson :: forall a. (Generic a) => a -> Json
gAesonEncodeJson = genericEncodeJson aesonOptions

-- | Decode `Json` representation of a value which has a `Generic` type
-- | with Aeson options. Data from Haskell, with Aeson default options can be
-- | decoded with gAesonDecodJson.
gAesonDecodeJson :: forall a. (Generic a) => Json -> Either String a
gAesonDecodeJson = genericDecodeJson aesonOptions


aesonUserEncoding :: Options -> GenericSignature -> GenericSpine -> Maybe Json
aesonUserEncoding opts sig spine = fromArray <$> encodeTuple opts sig spine

aesonUserDecoding :: Options -> GenericSignature -> Json -> Maybe GenericSpine
aesonUserDecoding _ _ _ = Nothing


encodeTuple :: Options -> GenericSignature ->  GenericSpine -> Maybe JArray
encodeTuple opts (SigProd "Data.Tuple.Tuple" sigArr) (SProd "Data.Tuple.Tuple" arr) =
    append
      <$> encodeTuple opts (Unsafe.head signatures) (Unsafe.head spines)
      <*> encodeTupleArgs opts (Unsafe.tail signatures) (Unsafe.tail spines)
  <|>
    encodeTupleArgs opts signatures spines -- Or just encode arguments
  where
    tupleC = Unsafe.head sigArr
    signatures = ($ unit) <$> tupleC.sigValues
    spines = ($ unit) <$> arr
encodeTuple _ _ _ = Nothing


encodeTupleArgs :: Options -> Array GenericSignature -> Array GenericSpine -> Maybe JArray
encodeTupleArgs opts sigs arr = return $ zipWith (genericUserEncodeJson' opts) sigs arr
