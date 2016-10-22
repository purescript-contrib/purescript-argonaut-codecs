module Data.Argonaut.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.Int (toNumber)
import Data.List (List(..), (:), toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (singleton)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

class EncodeJson a where
  encodeJson :: a -> Json

-- | Encode any `Generic` data structure into `Json`.
gEncodeJson :: forall a. Generic a => a -> Json
gEncodeJson = gEncodeJson' <<< toSpine

-- | Encode `GenericSpine` into `Json`.
gEncodeJson' :: GenericSpine -> Json
gEncodeJson' = case _ of
  SInt x -> fromNumber $ toNumber x
  SString x -> fromString x
  SChar x -> fromString $ singleton x
  SNumber x -> fromNumber x
  SBoolean x -> fromBoolean x
  SArray thunks -> fromArray (gEncodeJson' <<< (unit # _) <$> thunks)
  SUnit -> jsonNull
  SProd constr args ->
    fromObject
      $ SM.insert "tag" (encodeJson constr)
      $ SM.singleton "values" (encodeJson (gEncodeJson' <<< (unit # _) <$> args))
  SRecord fields ->
    fromObject $ foldr addField SM.empty fields
    where
    addField field =
      SM.insert field.recLabel (gEncodeJson' $ field.recValue unit)

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

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJson <<< singleton

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> json)

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = fromArray <<< map encodeJson <<< toUnfoldable

instance encodeStrMap :: EncodeJson a => EncodeJson (SM.StrMap a) where
  encodeJson = fromObject <<< map encodeJson

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< M.toList
