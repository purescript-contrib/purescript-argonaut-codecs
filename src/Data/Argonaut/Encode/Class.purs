module Data.Argonaut.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject, jsonEmptyObject, jsonSingletonObject)
import Data.Either (Either(), either)
import Data.Array as Arr
import Data.Int (toNumber)
import Data.List (List(..), (:), toUnfoldable)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String (singleton)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonEmptyObject
  encodeJson (Just a) = jsonSingletonObject "just" (encodeJson a)

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = either (obj "Left") (obj "Right")
    where
    obj :: forall c. EncodeJson c => String -> c -> Json
    obj tag x =
      fromObject $ SM.fromFoldable $
        Tuple "tag" (fromString tag) : Tuple "value" (encodeJson x) : Nil

instance encodeJsonUnit :: EncodeJson Unit where
  encodeJson = const jsonNull

instance encodeJsonJBoolean :: EncodeJson Boolean where
  encodeJson = fromBoolean

instance encodeJsonJNumber :: EncodeJson Number where
  encodeJson = fromNumber

instance encodeJsonInt :: EncodeJson Int where
  encodeJson = fromNumber <<< toNumber

instance encodeJsonJString :: EncodeJson String where
  encodeJson = fromString

instance encodeJsonJson :: EncodeJson Json where
  encodeJson = id

instance encodeJsonNonEmptyArray :: (EncodeJson a) => EncodeJson (NonEmpty Array a) where
  encodeJson (NonEmpty h t) = encodeJson $ Arr.cons h t

instance encodeJsonNonEmptyList :: (EncodeJson a) => EncodeJson (NonEmpty List a) where
  encodeJson (NonEmpty h t) = encodeJson $ L.insertAt 0 h t

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJson <<< singleton

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> json)

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = fromArray <<< map encodeJson <<< toUnfoldable

instance encodeStrMap :: EncodeJson a => EncodeJson (SM.StrMap a) where
  encodeJson = fromObject <<< map encodeJson

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

instance encodeVoid :: EncodeJson Void where
  encodeJson = absurd
