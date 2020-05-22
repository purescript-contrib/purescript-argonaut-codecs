module Data.Argonaut.Decode.Implementation where

import Prelude

import Data.Argonaut.Core (Json, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonString, isNull, stringify, toArray, toObject, toString)
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

type Decoder a = Json -> Either String a

decodeIdentity :: ∀ a . Decoder a -> Decoder (Identity a)
decodeIdentity decoder j = Identity <$> decoder j

decodeMaybe :: ∀ a . Decoder a -> Decoder (Maybe a)
decodeMaybe decoder j
  | isNull j = pure Nothing
  | otherwise = Just <$> decoder j

decodeTuple :: ∀ a b . Decoder a -> Decoder b -> Decoder (Tuple a b)
decodeTuple decoderA decoderB j = decodeArray Right j >>= f
  where
  f :: Array Json -> Either String (Tuple a b)
  f [a, b] = Tuple <$> decoderA a <*> decoderB b
  f _ = Left "Couldn't decode Tuple"

decodeEither :: ∀ a b . Decoder a -> Decoder b -> Decoder (Either a b)
decodeEither decoderA decoderB j =
  lmap ("Couldn't decode Either: " <> _) $
    decodeJObject j >>= \obj -> do
      tag <- maybe (Left "Expected field 'tag'") Right $ FO.lookup "tag" obj
      val <- maybe (Left "Expected field 'value'") Right $ FO.lookup "value" obj
      case toString tag of
        Just "Right" -> Right <$> decoderB val
        Just "Left" -> Left <$> decoderA val
        _ -> Left "'tag' field was not \"Left\" or \"Right\""

decodeNull :: Decoder Unit
decodeNull = caseJsonNull (Left "Value is not a null") (const $ Right unit)

decodeBoolean :: Decoder Boolean
decodeBoolean = caseJsonBoolean (Left "Value is not a Boolean") Right

decodeNumber :: Decoder Number
decodeNumber = caseJsonNumber (Left "Value is not a Number") Right

decodeInt :: Decoder Int
decodeInt =
  maybe (Left "Value is not an Integer") Right
    <<< fromNumber
    <=< decodeNumber

decodeString :: Decoder String
decodeString = caseJsonString (Left "Value is not a String") Right

decodeNonEmpty_Array :: ∀ a . Decoder a -> Decoder (NonEmpty Array a)
decodeNonEmpty_Array decoder =
  lmap ("Couldn't decode NonEmpty Array: " <> _)
    <<< (traverse decoder <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> x.head :| x.tail) <<< note " is empty" <<< Arr.uncons) <=< decodeJArray)

decodeNonEmptyArray :: ∀ a . Decoder a -> Decoder (NonEmptyArray a)
decodeNonEmptyArray decoder =
  lmap ("Couldn't decode NonEmptyArray: " <> _)
    <<< (traverse decoder <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> NEA.cons' x.head x.tail) <<< note " is empty" <<< Arr.uncons) <=< decodeJArray)

decodeNonEmpty_List :: ∀ a . Decoder a -> Decoder (NonEmpty List a)
decodeNonEmpty_List decoder =
  lmap ("Couldn't decode NonEmpty List: " <> _)
    <<< (traverse decoder <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> x.head :| x.tail) <<< note " is empty" <<< L.uncons) <=< map (map fromFoldable) decodeJArray)

decodeNonEmptyList :: ∀ a . Decoder a -> Decoder (NonEmptyList a)
decodeNonEmptyList decoder =
  lmap ("Couldn't decode NonEmptyList: " <> _)
    <<< (traverse decoder <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> NEL.cons' x.head x.tail) <<< note " is empty" <<< L.uncons) <=< map (map fromFoldable) decodeJArray)

decodeCodePoint :: Decoder CodePoint
decodeCodePoint j =
  maybe (Left $ "Expected character but found: " <> stringify j) Right
    =<< codePointAt 0 <$> decodeString j

decodeForeignObject :: ∀ a . Decoder a -> Decoder (FO.Object a)
decodeForeignObject decoder =
  lmap ("Couldn't decode ForeignObject: " <> _)
    <<< (traverse decoder <=< decodeJObject)

decodeArray :: ∀ a . Decoder a -> Decoder (Array a)
decodeArray decoder =
  lmap ("Couldn't decode Array (" <> _)
    <<< (traverseWithIndex f <=< decodeJArray)
  where
  msg i m = "Failed at index " <> show i <> "): " <> m
  f i = lmap (msg i) <<< decoder

decodeList :: ∀ a . Decoder a -> Decoder (List a)
decodeList decoder =
  lmap ("Couldn't decode List: " <> _)
    <<< (traverse decoder <=< map (map fromFoldable) decodeJArray)

decodeSet :: ∀ a . Ord a => Decoder a -> Decoder (S.Set a)
decodeSet decoder = map (S.fromFoldable :: List a -> S.Set a) <<< decodeList decoder

decodeMap :: ∀ a b . Ord a => Decoder a -> Decoder b -> Decoder (M.Map a b)
decodeMap decoderA decoderB = map (M.fromFoldable :: List (Tuple a b) -> M.Map a b) <<< decodeList (decodeTuple decoderA decoderB)

decodeVoid :: Decoder Void
decodeVoid _ = Left "Value cannot be Void"

decodeJArray :: Decoder (Array Json)
decodeJArray = maybe (Left "Value is not an Array") Right <<< toArray

decodeJObject :: Decoder (FO.Object Json)
decodeJObject = maybe (Left "Value is not an Object") Right <<< toObject

getField :: forall a. Decoder a -> FO.Object Json -> String -> Either String a
getField decoder o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decoder)
    (FO.lookup s o)

getFieldOptional :: forall a. Decoder a -> FO.Object Json -> String -> Either String (Maybe a)
getFieldOptional decoder o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< decoder) json

getFieldOptional' :: forall a. Decoder a -> FO.Object Json -> String -> Either String (Maybe a)
getFieldOptional' decoder o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json =
      if isNull json
        then pure Nothing
        else Just <$> (elaborateFailure s <<< decoder) json

elaborateFailure :: ∀ a. String -> Either String a -> Either String a
elaborateFailure s e =
  lmap msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m
