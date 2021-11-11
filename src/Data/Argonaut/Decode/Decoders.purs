module Data.Argonaut.Decode.Decoders where

import Prelude

import Data.Argonaut.Core (Json, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonString, isNull, toArray, toObject, toString, fromString)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
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
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Identity a)
decodeIdentity decoder json = Identity <$> decoder json

decodeMaybe
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Maybe a)
decodeMaybe decoder json
  | isNull json = pure Nothing
  | otherwise = Just <$> decoder json

decodeTuple
  :: forall a b
   . (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError (Tuple a b)
decodeTuple decoderA decoderB json = decodeArray Right json >>= f
  where
  f :: Array Json -> Either JsonDecodeError (Tuple a b)
  f = case _ of
    [ a, b ] -> Tuple <$> decoderA a <*> decoderB b
    _ -> Left $ TypeMismatch "Tuple"

decodeEither
  :: forall a b
   . (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError (Either a b)
decodeEither decoderA decoderB json =
  lmap (Named "Either") $ decodeJObject json >>= \obj -> do
    tag <- note (AtKey "tag" MissingValue) $ FO.lookup "tag" obj
    val <- note (AtKey "value" MissingValue) $ FO.lookup "value" obj
    case toString tag of
      Just "Right" -> Right <$> decoderB val
      Just "Left" -> Left <$> decoderA val
      _ -> Left $ AtKey "tag" (UnexpectedValue tag)

decodeNull :: Json -> Either JsonDecodeError Unit
decodeNull = caseJsonNull (Left $ TypeMismatch "null") (const $ Right unit)

decodeBoolean :: Json -> Either JsonDecodeError Boolean
decodeBoolean = caseJsonBoolean (Left $ TypeMismatch "Boolean") Right

decodeNumber :: Json -> Either JsonDecodeError Number
decodeNumber = caseJsonNumber (Left $ TypeMismatch "Number") Right

decodeInt :: Json -> Either JsonDecodeError Int
decodeInt = note (TypeMismatch "Integer") <<< fromNumber <=< decodeNumber

decodeString :: Json -> Either JsonDecodeError String
decodeString = caseJsonString (Left $ TypeMismatch "String") Right

decodeNonEmptyString :: Json -> Either JsonDecodeError NonEmptyString
decodeNonEmptyString json =
  note (Named "NonEmptyString" $ UnexpectedValue json)
    =<< map (NonEmptyString.fromString) (decodeString json)

decodeNonEmpty_Array
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (NonEmpty Array a)
decodeNonEmpty_Array decoder =
  lmap (Named "NonEmpty Array")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
      <<< note (TypeMismatch "NonEmpty Array")
      <<< Arr.uncons
    <=< decodeJArray

decodeNonEmptyArray
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (NonEmptyArray a)
decodeNonEmptyArray decoder =
  lmap (Named "NonEmptyArray")
    <<< traverse decoder
    <=< map (\x -> NEA.cons' x.head x.tail)
      <<< note (TypeMismatch "NonEmptyArray")
      <<< Arr.uncons
    <=< decodeJArray

decodeNonEmpty_List
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (NonEmpty List a)
decodeNonEmpty_List decoder =
  lmap (Named "NonEmpty List")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
      <<< note (TypeMismatch "NonEmpty List")
      <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeNonEmptyList
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (NonEmptyList a)
decodeNonEmptyList decoder =
  lmap (Named "NonEmptyList")
    <<< traverse decoder
    <=< map (\x -> NEL.cons' x.head x.tail)
      <<< note (TypeMismatch "NonEmptyList")
      <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeCodePoint :: Json -> Either JsonDecodeError CodePoint
decodeCodePoint json =
  note (Named "CodePoint" $ UnexpectedValue json)
    =<< map (codePointAt 0) (decodeString json)

decodeForeignObject
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (FO.Object a)
decodeForeignObject decoder =
  lmap (Named "ForeignObject")
    <<< traverse decoder
    <=< decodeJObject

decodeArray
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Array a)
decodeArray decoder =
  lmap (Named "Array")
    <<< traverseWithIndex (\i -> lmap (AtIndex i) <<< decoder)
    <=< decodeJArray

decodeList
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (List a)
decodeList decoder =
  lmap (Named "List")
    <<< traverse decoder
    <=< map (map fromFoldable) decodeJArray

decodeSet
  :: forall a
   . Ord a
  => (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (S.Set a)
decodeSet decoder =
  map (S.fromFoldable :: List a -> S.Set a) <<< decodeList decoder

decodeMap
  :: forall a b
   . Ord a
  => (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError (M.Map a b)
decodeMap decoderA decoderB =
  map (M.fromFoldable :: List (Tuple a b) -> M.Map a b)
    <<< decodeList (decodeTuple decoderA decoderB)

decodeVoid :: Json -> Either JsonDecodeError Void
decodeVoid _ = Left $ UnexpectedValue $ fromString "Value cannot be Void"

decodeJArray :: Json -> Either JsonDecodeError (Array Json)
decodeJArray = note (TypeMismatch "Array") <<< toArray

decodeJObject :: Json -> Either JsonDecodeError (FO.Object Json)
decodeJObject = note (TypeMismatch "Object") <<< toObject

getField
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> FO.Object Json
  -> String
  -> Either JsonDecodeError a
getField decoder obj str =
  maybe
    (Left $ AtKey str MissingValue)
    (lmap (AtKey str) <<< decoder)
    (FO.lookup str obj)

getFieldOptional
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> FO.Object Json
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional decoder obj str =
  maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
  where
  decode = lmap (AtKey str) <<< decoder

getFieldOptional'
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> FO.Object Json
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' decoder obj str =
  maybe (pure Nothing) decode (FO.lookup str obj)
  where
  decode json =
    if isNull json then
      pure Nothing
    else
      Just <$> (lmap (AtKey str) <<< decoder) json
