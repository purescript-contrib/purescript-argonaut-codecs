module Data.Argonaut.Decode.Decoders where

import Prelude

import Data.Argonaut.Core (Json, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonString, isNull, toArray, toObject, toString, fromString)
import Data.Argonaut.Decode.Error (JsonDecodeError'(..))
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Identity (Identity(..))
import Data.Int (fromNumber)
import Data.List (List, fromFoldable)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Map as M
import Data.Maybe (maybe, Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set as S
import Data.String (CodePoint, codePointAt)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

decodeIdentity
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (Identity a)
decodeIdentity decoder json = Identity <$> decoder json

decodeMaybe
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (Maybe a)
decodeMaybe decoder json
  | isNull json = pure Nothing
  | otherwise = Just <$> decoder json

decodeTuple
  :: forall customErr a b
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> (Json -> Either (JsonDecodeError' customErr) b)
  -> Json
  -> Either (JsonDecodeError' customErr) (Tuple a b)
decodeTuple decoderA decoderB json = decodeArray Right json >>= f
  where
  f :: Array Json -> Either (JsonDecodeError' customErr) (Tuple a b)
  f = case _ of
    [ a, b ] -> Tuple <$> decoderA a <*> decoderB b
    _ -> Left $ TypeMismatch "Tuple"

decodeEither
  :: forall customErr a b
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> (Json -> Either (JsonDecodeError' customErr) b)
  -> Json
  -> Either (JsonDecodeError' customErr) (Either a b)
decodeEither decoderA decoderB json =
  lmap (Named "Either") $ decodeJObject json >>= \obj -> do
    tag <- note (AtKey "tag" MissingValue) $ FO.lookup "tag" obj
    val <- note (AtKey "value" MissingValue) $ FO.lookup "value" obj
    case toString tag of
      Just "Right" -> Right <$> decoderB val
      Just "Left" -> Left <$> decoderA val
      _ -> Left $ AtKey "tag" (UnexpectedValue tag)

decodeNull :: forall customErr. Json -> Either (JsonDecodeError' customErr) Unit
decodeNull = caseJsonNull (Left $ TypeMismatch "null") (const $ Right unit)

decodeBoolean :: forall customErr. Json -> Either (JsonDecodeError' customErr) Boolean
decodeBoolean = caseJsonBoolean (Left $ TypeMismatch "Boolean") Right

decodeNumber :: forall customErr. Json -> Either (JsonDecodeError' customErr) Number
decodeNumber = caseJsonNumber (Left $ TypeMismatch "Number") Right

decodeInt :: forall customErr. Json -> Either (JsonDecodeError' customErr) Int
decodeInt = note (TypeMismatch "Integer") <<< fromNumber <=< decodeNumber

decodeString :: forall customErr. Json -> Either (JsonDecodeError' customErr) String
decodeString = caseJsonString (Left $ TypeMismatch "String") Right

decodeNonEmptyString :: forall customErr. Json -> Either (JsonDecodeError' customErr) NonEmptyString
decodeNonEmptyString json =
  note (Named "NonEmptyString" $ UnexpectedValue json)
    =<< map (NonEmptyString.fromString) (decodeString json)

decodeNonEmpty_Array
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (NonEmpty Array a)
decodeNonEmpty_Array decoder =
  lmap (Named "NonEmpty Array")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
      <<< note (TypeMismatch "NonEmpty Array")
      <<< Arr.uncons
    <=< decodeJArray

decodeNonEmptyArray
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (NonEmptyArray a)
decodeNonEmptyArray decoder =
  lmap (Named "NonEmptyArray")
    <<< traverse decoder
    <=< map (\x -> NEA.cons' x.head x.tail)
      <<< note (TypeMismatch "NonEmptyArray")
      <<< Arr.uncons
    <=< decodeJArray

decodeNonEmpty_List
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (NonEmpty List a)
decodeNonEmpty_List decoder =
  lmap (Named "NonEmpty List")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
      <<< note (TypeMismatch "NonEmpty List")
      <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeNonEmptyList
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (NonEmptyList a)
decodeNonEmptyList decoder =
  lmap (Named "NonEmptyList")
    <<< traverse decoder
    <=< map (\x -> NEL.cons' x.head x.tail)
      <<< note (TypeMismatch "NonEmptyList")
      <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeCodePoint :: forall customErr. Json -> Either (JsonDecodeError' customErr) CodePoint
decodeCodePoint json =
  note (Named "CodePoint" $ UnexpectedValue json)
    =<< map (codePointAt 0) (decodeString json)

decodeForeignObject
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (FO.Object a)
decodeForeignObject decoder =
  lmap (Named "ForeignObject")
    <<< traverse decoder
    <=< decodeJObject

decodeArray
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (Array a)
decodeArray decoder =
  lmap (Named "Array")
    <<< traverseWithIndex (\i -> lmap (AtIndex i) <<< decoder)
    <=< decodeJArray

decodeList
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (List a)
decodeList decoder =
  lmap (Named "List")
    <<< traverse decoder
    <=< map (map fromFoldable) decodeJArray

decodeSet
  :: forall customErr a
   . Ord a
  => (Json -> Either (JsonDecodeError' customErr) a)
  -> Json
  -> Either (JsonDecodeError' customErr) (S.Set a)
decodeSet decoder =
  map (S.fromFoldable :: List a -> S.Set a) <<< decodeList decoder

decodeMap
  :: forall customErr a b
   . Ord a
  => (Json -> Either (JsonDecodeError' customErr) a)
  -> (Json -> Either (JsonDecodeError' customErr) b)
  -> Json
  -> Either (JsonDecodeError' customErr) (M.Map a b)
decodeMap decoderA decoderB =
  map (M.fromFoldable :: List (Tuple a b) -> M.Map a b)
    <<< decodeList (decodeTuple decoderA decoderB)

decodeVoid :: forall customErr. Json -> Either (JsonDecodeError' customErr) Void
decodeVoid _ = Left $ UnexpectedValue $ fromString "Value cannot be Void"

decodeJArray :: forall customErr. Json -> Either (JsonDecodeError' customErr) (Array Json)
decodeJArray = note (TypeMismatch "Array") <<< toArray

decodeJObject :: forall customErr. Json -> Either (JsonDecodeError' customErr) (FO.Object Json)
decodeJObject = note (TypeMismatch "Object") <<< toObject

getField
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> FO.Object Json
  -> String
  -> Either (JsonDecodeError' customErr) a
getField decoder obj str =
  maybe
    (Left $ AtKey str MissingValue)
    (lmap (AtKey str) <<< decoder)
    (FO.lookup str obj)

getFieldOptional
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> FO.Object Json
  -> String
  -> Either (JsonDecodeError' customErr) (Maybe a)
getFieldOptional decoder obj str =
  maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
  where
  decode = lmap (AtKey str) <<< decoder

getFieldOptional'
  :: forall customErr a
   . (Json -> Either (JsonDecodeError' customErr) a)
  -> FO.Object Json
  -> String
  -> Either (JsonDecodeError' customErr) (Maybe a)
getFieldOptional' decoder obj str =
  maybe (pure Nothing) decode (FO.lookup str obj)
  where
  decode json =
    if isNull json then
      pure Nothing
    else
      Just <$> (lmap (AtKey str) <<< decoder) json
