module Data.Argonaut.Encode
       ( EncodeJson
       , encodeJson
       ) where

import Prelude

import Data.Argonaut.Core
  ( Json(..)
  , foldJsonObject
  , jsonNull
  , fromNull
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  , jsonEmptyArray
  , jsonEmptyObject
  , jsonSingletonObject
  )
import Data.String (fromChar)
import Data.Maybe
import Data.Either
import Data.List (List(..), fromList)
import Data.Int (toNumber)
import Data.Unfoldable ()
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))

import qualified Data.StrMap as SM
import qualified Data.Map as M

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonNull
  encodeJson (Just a) = encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson (Left a) = encodeJson a
  encodeJson (Right b) = encodeJson b

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
  encodeJson = encodeJson <<< fromChar

instance encodeJsonArray :: (EncodeJson a) => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> json)

instance encodeJsonList :: (EncodeJson a) => EncodeJson (List a) where
  encodeJson json =
    let arr :: Array a
        arr = fromList json
    in fromArray (encodeJson <$> arr)

instance encodeStrMap :: (EncodeJson a) => EncodeJson (SM.StrMap a) where
  encodeJson m = fromObject (encodeJson <$> m)

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< M.toList

