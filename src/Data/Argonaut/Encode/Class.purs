module Data.Argonaut.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Either (Either, either)
import Data.Int (toNumber)
import Data.List (List(..), (:), toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonNull
  encodeJson (Just a) = encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = either (obj "Left") (obj "Right")
    where
    obj :: forall c. EncodeJson c => String -> c -> Json
    obj tag x =
      fromObject $ FO.fromFoldable $
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
  encodeJson = identity

instance encodeJsonCodePoint :: EncodeJson CodePoint where
  encodeJson = encodeJson <<< CP.singleton

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJson <<< CU.singleton

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> json)

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = fromArray <<< map encodeJson <<< toUnfoldable

instance encodeForeignObject :: EncodeJson a => EncodeJson (FO.Object a) where
  encodeJson = fromObject <<< map encodeJson

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

instance encodeVoid :: EncodeJson Void where
  encodeJson = absurd
