module Data.Argonaut.Decode.Class
  ( class DecodeJson
  , decodeJson
  ) where

import Prelude

import Data.Argonaut.Core (Json, JArray, JObject, isNull, foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toObject, toString)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.List (List(..), (:), fromFoldable)
import Data.Map as M
import Data.Maybe (maybe, Maybe(..))
import Data.String (charAt)
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeJsonMaybe :: DecodeJson a => DecodeJson (Maybe a) where
  decodeJson j
    | isNull j = pure Nothing
    | otherwise = Just <$> decodeJson j

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f
    where
    f (a : b : Nil) = Tuple <$> decodeJson a <*> decodeJson b
    f _ = Left "Couldn't decode Tuple"

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson json =
    lmap ("Couldn't decode Either: " <> _) $
      decodeJObject json >>= \obj -> do
        tag <- maybe (Left "Expected field 'tag'") Right $ SM.lookup "tag" obj
        val <- maybe (Left "Expected field 'value'") Right $ SM.lookup "value" obj
        case toString tag of
          Just "Right" -> Right <$> decodeJson val
          Just "Left" -> Left <$> decodeJson val
          _ -> Left "'tag' field was not \"Left\" or \"Right\""

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = foldJsonNull (Left "Value is not a null") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = foldJsonBoolean (Left "Value is not a Boolean") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = foldJsonNumber (Left "Value is not a Number") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson
    = maybe (Left "Value is not an integer") Right
    <<< fromNumber
    <=< decodeJson

instance decodeJsonString :: DecodeJson String where
  decodeJson = foldJsonString (Left "Value is not a String") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonChar :: DecodeJson Char where
  decodeJson j =
    maybe (Left $ "Expected character but found: " <> show j) Right
      =<< charAt 0 <$> decodeJson j

instance decodeStrMap :: DecodeJson a => DecodeJson (SM.StrMap a) where
  decodeJson
    = lmap ("Couldn't decode StrMap: " <> _)
    <<< (traverse decodeJson <=< decodeJObject)

instance decodeArray :: DecodeJson a => DecodeJson (Array a) where
  decodeJson
    = lmap ("Couldn't decode Array: " <> _)
    <<< (traverse decodeJson <=< decodeJArray)

instance decodeList :: DecodeJson a => DecodeJson (List a) where
  decodeJson
    = lmap ("Couldn't decode List: " <> _)
    <<< (traverse decodeJson <=< map (map fromFoldable) decodeJArray)

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (M.Map a b) where
  decodeJson = map (M.fromFoldable :: List (Tuple a b) -> M.Map a b) <<< decodeJson

instance decodeVoid :: DecodeJson Void where
  decodeJson _ = Left "Value cannot be Void"

decodeJArray :: Json -> Either String JArray
decodeJArray = maybe (Left "Value is not an Array") Right <<< toArray

decodeJObject :: Json -> Either String JObject
decodeJObject = maybe (Left "Value is not an Object") Right <<< toObject
