module Data.Argonaut.Decode.Decoders where

import Prelude

import Data.Argonaut.Core (Json, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonString, isNull, toArray, toObject, toString, fromString)
import Data.Argonaut.Decode.Errors (JsonDecodeError(..))
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), note)
import Data.Identity (Identity(..))
import Data.Int (fromNumber)
import Data.List (List, fromFoldable)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Maybe (maybe, Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set as S
import Data.String (CodePoint, codePointAt)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

decodeIdentity :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Identity a)
decodeIdentity decoder j = Identity <$> decoder j

decodeMaybe :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Maybe a)
decodeMaybe decoder j
  | isNull j = pure Nothing
  | otherwise = Just <$> decoder j

decodeTuple :: ∀ a b . (Json -> Either JsonDecodeError a) -> (Json -> Either JsonDecodeError b) -> Json -> Either JsonDecodeError (Tuple a b)
decodeTuple decoderA decoderB j = decodeArray Right j >>= f
  where
  f :: Array Json -> Either JsonDecodeError (Tuple a b)
  f [a, b] = Tuple <$> decoderA a <*> decoderB b
  f _ = Left $ TypeMismatch "Tuple"

decodeEither :: ∀ a b . (Json -> Either JsonDecodeError a) -> (Json -> Either JsonDecodeError b) -> Json -> Either JsonDecodeError (Either a b)
decodeEither decoderA decoderB j =
  lmap (Named "Either") $
    decodeJObject j >>= \obj -> do
      tag <- maybe (Left $ AtKey "tag" MissingValue) Right $ FO.lookup "tag" obj
      val <- maybe (Left $ AtKey "value" MissingValue) Right $ FO.lookup "value" obj
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
decodeInt =
  maybe (Left $ TypeMismatch "Integer") Right
    <<< fromNumber
    <=< decodeNumber

decodeString :: Json -> Either JsonDecodeError String
decodeString = caseJsonString (Left $ TypeMismatch "String") Right

decodeNonEmpty_Array :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (NonEmpty Array a)
decodeNonEmpty_Array decoder =
  lmap (Named "NonEmpty Array")
    <<< (traverse decoder <=< (rmap (\x -> x.head :| x.tail) <<< note (TypeMismatch "NonEmpty Array") <<< Arr.uncons) <=< decodeJArray)

decodeNonEmptyArray :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (NonEmptyArray a)
decodeNonEmptyArray decoder =
  lmap (Named "NonEmptyArray")
    <<< (traverse decoder <=< (rmap (\x -> NEA.cons' x.head x.tail) <<< note (TypeMismatch "NonEmptyArray") <<< Arr.uncons) <=< decodeJArray)

decodeNonEmpty_List :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (NonEmpty List a)
decodeNonEmpty_List decoder =
  lmap (Named "NonEmpty List")
    <<< (traverse decoder <=< (rmap (\x -> x.head :| x.tail) <<< note (TypeMismatch "NonEmpty List") <<< L.uncons) <=< map (map fromFoldable) decodeJArray)

decodeNonEmptyList :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (NonEmptyList a)
decodeNonEmptyList decoder =
  lmap (Named "NonEmptyList")
    <<< (traverse decoder <=< (rmap (\x -> NEL.cons' x.head x.tail) <<< note (TypeMismatch "NonEmptyList") <<< L.uncons) <=< map (map fromFoldable) decodeJArray)

decodeCodePoint :: Json -> Either JsonDecodeError CodePoint
decodeCodePoint j =
  maybe (Left $ Named "CodePoint" $ UnexpectedValue j) Right
    =<< codePointAt 0 <$> decodeString j

decodeForeignObject :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (FO.Object a)
decodeForeignObject decoder =
  lmap (Named "ForeignObject")
    <<< (traverse decoder <=< decodeJObject)

decodeArray :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Array a)
decodeArray decoder =
  lmap (Named "Array")
    <<< (traverseWithIndex f <=< decodeJArray)
  where
  msg i m = AtIndex i m
  f i = lmap (msg i) <<< decoder

decodeList :: ∀ a . (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (List a)
decodeList decoder =
  lmap (Named "List")
    <<< (traverse decoder <=< map (map fromFoldable) decodeJArray)

decodeSet :: ∀ a . Ord a => (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (S.Set a)
decodeSet decoder = map (S.fromFoldable :: List a -> S.Set a) <<< decodeList decoder

decodeMap :: ∀ a b . Ord a => (Json -> Either JsonDecodeError a) -> (Json -> Either JsonDecodeError b) -> Json -> Either JsonDecodeError (M.Map a b)
decodeMap decoderA decoderB = map (M.fromFoldable :: List (Tuple a b) -> M.Map a b) <<< decodeList (decodeTuple decoderA decoderB)

decodeVoid :: Json -> Either JsonDecodeError Void
decodeVoid _ = Left $ UnexpectedValue $ fromString "Value cannot be Void"

decodeJArray :: Json -> Either JsonDecodeError (Array Json)
decodeJArray = maybe (Left $ TypeMismatch "Array") Right <<< toArray

decodeJObject :: Json -> Either JsonDecodeError (FO.Object Json)
decodeJObject = maybe (Left $ TypeMismatch "Object") Right <<< toObject

getField :: forall a. (Json -> Either JsonDecodeError a) -> FO.Object Json -> String -> Either JsonDecodeError a
getField decoder o s =
  maybe
    (Left $ AtKey s MissingValue)
    (lmap (AtKey s) <<< decoder)
    (FO.lookup s o)

getFieldOptional :: forall a. (Json -> Either JsonDecodeError a) -> FO.Object Json -> String -> Either JsonDecodeError (Maybe a)
getFieldOptional decoder o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json = Just <$> (lmap (AtKey s) <<< decoder) json

getFieldOptional' :: forall a. (Json -> Either JsonDecodeError a) -> FO.Object Json -> String -> Either JsonDecodeError (Maybe a)
getFieldOptional' decoder o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json =
      if isNull json
        then pure Nothing
        else Just <$> (lmap (AtKey s) <<< decoder) json
