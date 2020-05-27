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
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Set as S
import Data.String (CodePoint)
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

type Encoder a = a -> Json

encodeIdentity :: forall a . Encoder a -> Encoder (Identity a)
encodeIdentity encoder (Identity a) = encoder a

encodeMaybe :: forall a . Encoder a -> Encoder (Maybe a)
encodeMaybe encoder = case _ of
    Nothing -> jsonNull
    Just a -> encoder a

encodeTuple :: forall a b . Encoder a -> Encoder b -> Encoder (Tuple a b)
encodeTuple encoderA encoderB (Tuple a b) = fromArray [encoderA a, encoderB b]

encodeEither :: forall a b . Encoder a -> Encoder b -> Encoder (Either a b)
encodeEither encoderA encoderB = either (obj encoderA "Left") (obj encoderB "Right")
    where
    obj :: forall c. Encoder c -> String -> c -> Json
    obj encoder tag x =
      fromObject
        $ FO.fromFoldable
        $ Tuple "tag" (fromString tag) : Tuple "value" (encoder x) : Nil

encodeUnit :: Encoder Unit
encodeUnit = const jsonNull

encodeBoolean :: Encoder Boolean
encodeBoolean = fromBoolean

encodeNumber :: Encoder Number
encodeNumber = fromNumber

encodeInt :: Encoder Int
encodeInt = fromNumber <<< toNumber

encodeString :: Encoder String
encodeString = fromString

encodeCodePoint :: Encoder CodePoint
encodeCodePoint = encodeString <<< CP.singleton

encodeNonEmpty_Array :: forall a . (Encoder a) -> Encoder (NonEmpty Array a)
encodeNonEmpty_Array encoder (NonEmpty h t) = encodeArray encoder (Arr.cons h t)

encodeNonEmptyArray :: forall a . (Encoder a) -> Encoder (NonEmptyArray a)
encodeNonEmptyArray encoder = encodeArray encoder <<< NEA.toArray

encodeNonEmpty_List :: forall a . (Encoder a) -> Encoder (NonEmpty List a)
encodeNonEmpty_List encoder (NonEmpty h t) = encodeList encoder (h : t)

encodeNonEmptyList :: forall a . (Encoder a) -> Encoder (NonEmptyList a)
encodeNonEmptyList encoder = encodeList encoder <<< NEL.toList

encodeChar :: Encoder Char
encodeChar = encodeString <<< CU.singleton

encodeArray :: forall a . Encoder a -> Encoder (Array a)
encodeArray encoder = fromArray <<< map encoder

encodeList :: forall a . Encoder a -> Encoder (List a)
encodeList encoder = fromArray <<< map encoder <<< toUnfoldable

encodeForeignObject :: forall a . Encoder a -> Encoder (FO.Object a)
encodeForeignObject encoder = fromObject <<< map encoder

encodeSet :: forall a . (Ord a) => Encoder a -> Encoder (S.Set a)
encodeSet encoder = encodeList encoder <<< (S.toUnfoldable :: S.Set a -> List a)

encodeMap :: forall a b . (Ord a) => Encoder a -> Encoder b -> Encoder (M.Map a b)
encodeMap encoderA encoderB = encodeList (encodeTuple encoderA encoderB) <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

encodeVoid :: Encoder Void
encodeVoid = absurd

assoc :: forall a. Encoder a -> String -> a -> Tuple String Json
assoc encoder k = Tuple k <<< encoder

assocOptional
  :: forall a
   . Encoder a
  -> String
  -> Maybe a
  -> Maybe (Tuple String Json)
assocOptional encoder k = (<$>) (Tuple k <<< encoder)

extend :: forall a. Encoder a -> Tuple String Json -> a -> Json
extend encoder (Tuple k v) =
  caseJsonObject
    (jsonSingletonObject k v)
    (FO.insert k v >>> fromObject)
    <<< encoder

-- | The named Encoders of the `(~>?)` operator.
extendOptional :: forall a. Encoder a -> Maybe (Tuple String Json) -> a -> Json
extendOptional encoder (Just kv) = extend encoder kv
extendOptional encoder Nothing = encoder
