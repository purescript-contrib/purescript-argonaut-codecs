module Data.Argonaut.Encode.Implementation where

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

encodeJsonMaybe :: forall a . Encoder a -> Encoder (Maybe a)
encodeJsonMaybe encoder = case _ of
    Nothing -> jsonNull
    Just a -> encoder a

encodeJsonTuple :: forall a b . Encoder a -> Encoder b -> Encoder (Tuple a b)
encodeJsonTuple encoderA encoderB (Tuple a b) = fromArray [encoderA a, encoderB b]

encodeJsonEither :: forall a b . Encoder a -> Encoder b -> Encoder (Either a b)
encodeJsonEither encoderA encoderB = either (obj encoderA "Left") (obj encoderB "Right")
    where
    obj :: forall c. Encoder c -> String -> c -> Json
    obj encoder tag x =
      fromObject
        $ FO.fromFoldable
        $ Tuple "tag" (fromString tag) : Tuple "value" (encoder x) : Nil

encodeJsonUnit :: Encoder Unit
encodeJsonUnit = const jsonNull

encodeJsonJBoolean :: Encoder Boolean
encodeJsonJBoolean = fromBoolean

encodeJsonJNumber :: Encoder Number
encodeJsonJNumber = fromNumber

encodeJsonInt :: Encoder Int
encodeJsonInt = fromNumber <<< toNumber

encodeJsonJString :: Encoder String
encodeJsonJString = fromString

encodeJsonJson :: Encoder Json
encodeJsonJson = identity

encodeJsonCodePoint :: Encoder CodePoint
encodeJsonCodePoint = encodeJsonJString <<< CP.singleton

encodeJsonNonEmpty_Array :: forall a . (Encoder a) -> Encoder (NonEmpty Array a)
encodeJsonNonEmpty_Array encoder (NonEmpty h t) = encodeJsonArray encoder (Arr.cons h t)

encodeJsonNonEmptyArray :: forall a . (Encoder a) -> Encoder (NonEmptyArray a)
encodeJsonNonEmptyArray encoder = encodeJsonArray encoder <<< NEA.toArray

encodeJsonNonEmpty_List :: forall a . (Encoder a) -> Encoder (NonEmpty List a)
encodeJsonNonEmpty_List encoder (NonEmpty h t) = encodeJsonList encoder (h : t)

encodeJsonNonEmptyList :: forall a . (Encoder a) -> Encoder (NonEmptyList a)
encodeJsonNonEmptyList encoder = encodeJsonList encoder <<< NEL.toList

encodeJsonChar :: Encoder Char
encodeJsonChar = encodeJsonJString <<< CU.singleton

encodeJsonArray :: forall a . Encoder a -> Encoder (Array a)
encodeJsonArray encoder = fromArray <<< map encoder

encodeJsonList :: forall a . Encoder a -> Encoder (List a)
encodeJsonList encoder = fromArray <<< map encoder <<< toUnfoldable

encodeForeignObject :: forall a . Encoder a -> Encoder (FO.Object a)
encodeForeignObject encoder = fromObject <<< map encoder

encodeSet :: forall a . (Ord a) => Encoder a -> Encoder (S.Set a)
encodeSet encoder = encodeJsonList encoder <<< (S.toUnfoldable :: S.Set a -> List a)

encodeMap :: forall a b . (Ord a) => Encoder a -> Encoder b -> Encoder (M.Map a b)
encodeMap encoderA encoderB = encodeJsonList (encodeJsonTuple encoderA encoderB) <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

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

-- | The named implementation of the `(~>?)` operator.
extendOptional :: forall a. Encoder a -> Maybe (Tuple String Json) -> a -> Json
extendOptional encoder (Just kv) = extend encoder kv
extendOptional encoder Nothing = encoder
