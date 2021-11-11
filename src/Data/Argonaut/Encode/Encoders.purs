module Data.Argonaut.Encode.Encoders where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, caseJsonObject, jsonSingletonObject)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List(..), (:), toUnfoldable)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Set as S
import Data.String (CodePoint)
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

encodeIdentity :: forall a. (a -> Json) -> Identity a -> Json
encodeIdentity encoder (Identity a) = encoder a

encodeMaybe :: forall a. (a -> Json) -> Maybe a -> Json
encodeMaybe encoder = case _ of
  Nothing -> jsonNull
  Just a -> encoder a

encodeTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
encodeTuple encoderA encoderB (Tuple a b) = fromArray [ encoderA a, encoderB b ]

encodeEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
encodeEither encoderA encoderB = either (obj encoderA "Left") (obj encoderB "Right")
  where
  obj :: forall c. (c -> Json) -> String -> c -> Json
  obj encoder tag x =
    fromObject
      $ FO.fromFoldable
      $ Tuple "tag" (fromString tag) : Tuple "value" (encoder x) : Nil

encodeUnit :: Unit -> Json
encodeUnit = const jsonNull

encodeBoolean :: Boolean -> Json
encodeBoolean = fromBoolean

encodeNumber :: Number -> Json
encodeNumber = fromNumber

encodeInt :: Int -> Json
encodeInt = fromNumber <<< toNumber

encodeString :: String -> Json
encodeString = fromString

encodeCodePoint :: CodePoint -> Json
encodeCodePoint = encodeString <<< CP.singleton

encodeNonEmptyString :: NonEmptyString -> Json
encodeNonEmptyString = fromString <<< NonEmptyString.toString

encodeNonEmpty_Array :: forall a. (a -> Json) -> NonEmpty Array a -> Json
encodeNonEmpty_Array encoder (NonEmpty h t) = encodeArray encoder (Arr.cons h t)

encodeNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
encodeNonEmptyArray encoder = encodeArray encoder <<< NEA.toArray

encodeNonEmpty_List :: forall a. (a -> Json) -> NonEmpty List a -> Json
encodeNonEmpty_List encoder (NonEmpty h t) = encodeList encoder (h : t)

encodeNonEmptyList :: forall a. (a -> Json) -> NonEmptyList a -> Json
encodeNonEmptyList encoder = encodeList encoder <<< NEL.toList

encodeChar :: Char -> Json
encodeChar = encodeString <<< CU.singleton

encodeArray :: forall a. (a -> Json) -> Array a -> Json
encodeArray encoder = fromArray <<< map encoder

encodeList :: forall a. (a -> Json) -> List a -> Json
encodeList encoder = fromArray <<< map encoder <<< toUnfoldable

encodeForeignObject :: forall a. (a -> Json) -> FO.Object a -> Json
encodeForeignObject encoder = fromObject <<< map encoder

encodeSet :: forall a. Ord a => (a -> Json) -> S.Set a -> Json
encodeSet encoder = encodeList encoder <<< (S.toUnfoldable :: S.Set a -> List a)

encodeMap :: forall a b. Ord a => (a -> Json) -> (b -> Json) -> M.Map a b -> Json
encodeMap encoderA encoderB =
  encodeList (encodeTuple encoderA encoderB)
    <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

encodeVoid :: Void -> Json
encodeVoid = absurd

assoc :: forall a. (a -> Json) -> String -> a -> Tuple String Json
assoc encoder k = Tuple k <<< encoder

assocOptional
  :: forall a
   . (a -> Json)
  -> String
  -> Maybe a
  -> Maybe (Tuple String Json)
assocOptional encoder k = map (Tuple k <<< encoder)

extend :: forall a. (a -> Json) -> Tuple String Json -> a -> Json
extend encoder (Tuple k v) =
  caseJsonObject (jsonSingletonObject k v) (fromObject <<< FO.insert k v)
    <<< encoder

-- | The named Encoders of the `(~>?)` operator.
extendOptional :: forall a. (a -> Json) -> Maybe (Tuple String Json) -> a -> Json
extendOptional encoder = case _ of
  Just kv -> extend encoder kv
  Nothing -> encoder
