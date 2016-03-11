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
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject, JArray, jsonNull, isNull, toObject, toArray)
import Data.Argonaut.Options
import Data.Argonaut.Encode
import Data.Argonaut.Decode hiding (decodeMaybe)
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor(), toSignature)
import Data.Int (toNumber)
import Data.List (List(..), fromList)
import Data.List as L
import Data.Map as M
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromChar)
import Data.StrMap as SM
import Data.Traversable (sequence)
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
aesonUserEncoding opts sig spine = encodeMaybe opts sig spine
                               <|> encodeEither opts sig spine
                               <|> fromArray <$> encodeTuple opts sig spine

aesonUserDecoding :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
aesonUserDecoding opts sig json = decodeMaybe opts sig json
                              <|> decodeEither opts sig json
                              <|> decodeTuple opts sig json


encodeMaybe :: Options -> GenericSignature -> GenericSpine -> Maybe Json
encodeMaybe opts (SigProd "Data.Maybe.Maybe" sigArr) (SProd "Data.Maybe.Just" [elem]) =
    return $ genericUserEncodeJson' opts valSig val
  where
    valSig = getSigFromUnaryConstructor sigArr "Data.Maybe.Just"
    val = elem unit

encodeMaybe opts (SigProd "Data.Maybe.Maybe" _) (SProd "Data.Maybe.Nothing" _) =
    return jsonNull
encodeMaybe _ _ _ = Nothing

decodeMaybe :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
decodeMaybe opts (SigProd "Data.Maybe.Maybe" sigArr) json =
  if isNull json
    then return $ Right $ SProd "Data.Maybe.Nothing" []
    else return $ do
      let valSig = getSigFromUnaryConstructor sigArr "Data.Maybe.Just"
      decoded <- genericUserDecodeJson' opts valSig json
      return $ SProd "Data.Maybe.Just" [\u -> decoded ]
decodeMaybe _ _ _ = Nothing

encodeEither :: Options -> GenericSignature -> GenericSpine -> Maybe Json
encodeEither opts (SigProd "Data.Either.Either" sigArr) (SProd eitherConstr [elem]) =
    return
      $ fromObject $ SM.fromList
      $ Tuple strippedConstr (genericUserEncodeJson' opts valSig val) `Cons` Nil
  where
    strippedConstr = stripModulePath eitherConstr
    valSig = getSigFromUnaryConstructor sigArr eitherConstr
    val = elem unit
encodeEither _ _ _ = Nothing

decodeEither :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
decodeEither opts (SigProd "Data.Either.Either" sigArr) json = return $ do
    obj <- mFail "Expeced an object when decoding Either" $ toObject json
    fromMaybe (Left "Expected Left or Right record label when decoding Either")
      $ decodeArg "Right" obj <|> decodeArg "Left" obj
  where
    decodeArg name obj = do
      argJson <- SM.lookup name obj
      let valSig = getSigFromUnaryConstructor sigArr $ "Data.Either." <> name
      return $ do
        decoded <- genericUserDecodeJson' opts valSig argJson
        return $ SProd ("Data.Either." <> name) [\u -> decoded]
decodeEither _ _ _ = Nothing

encodeTuple :: Options -> GenericSignature ->  GenericSpine -> Maybe JArray
encodeTuple opts (SigProd "Data.Tuple.Tuple" sigArr) (SProd "Data.Tuple.Tuple" arr) =
    append
      <$> encodeTuple opts (Unsafe.head signatures) (Unsafe.head spines)
      <*> encodeTupleArgs opts (Unsafe.tail signatures) (Unsafe.tail spines)
  <|>
    encodeTupleArgs opts signatures spines -- Or just encode arguments
  where
    signatures = getSigsFromConstructor sigArr "Data.Tuple.Tuple"
    spines = map ($ unit) arr
encodeTuple _ _ _ = Nothing

decodeTuple :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
decodeTuple opts (SigProd "Data.Tuple.Tuple" sigArr) json = return $ do
    jsonVals <- mFail "Expected an array of values when decoding Tuple" $ toArray json
    let sigs = getNestedTupleSigs $ getSigsFromConstructor sigArr "Data.Tuple.Tuple"
    decoded <- sequence $ L.zipWith (genericUserDecodeJson' opts) sigs (arrToList jsonVals)
    makeTuples decoded
  where
    makeTuple x1 x2 = SProd "Data.Tuple.Tuple" [\_ -> x1, \_ -> x2]

    makeTuples (Cons x1 (Cons x2 xs)) = return $ makeTuples' (makeTuple x1 x2) xs
    makeTuples _ = Left "A tuple needs to have at least two elements"

    makeTuples' inner Nil = inner
    makeTuples' inner (Cons x1 xs) = makeTuples' (makeTuple inner x1) xs


decodeTuple _ _ _ = Nothing

encodeTupleArgs :: Options -> Array GenericSignature -> Array GenericSpine -> Maybe JArray
encodeTupleArgs opts sigs arr = return $ zipWith (genericUserEncodeJson' opts) sigs arr


getSigFromUnaryConstructor :: Array DataConstructor -> String -> GenericSignature
getSigFromUnaryConstructor arr name = Unsafe.head $ getSigsFromConstructor arr name

getSigsFromConstructor :: Array DataConstructor -> String -> Array GenericSignature
getSigsFromConstructor arr name =
  let
    constr = Unsafe.head <<< filter ((== name) <<< _.sigConstructor) $ arr
  in
    map ($ unit) constr.sigValues

getNestedTupleSigs :: Array GenericSignature -> List GenericSignature
getNestedTupleSigs = L.reverse <<< getNestedTupleSigs'

-- Get signatures in reverse order:
getNestedTupleSigs' :: Array GenericSignature -> List GenericSignature
getNestedTupleSigs' [val1, val2] = case val1 of
  SigProd "Data.Tuple.Tuple" cVals -> val2 `Cons` getNestedTupleSigs' (getSigsFromConstructor cVals "Data.Tuple.Tuple")
  _                                -> Cons val2 (Cons val1 Nil)

arrToList :: forall a. Array a -> List a
arrToList = foldr Cons Nil
