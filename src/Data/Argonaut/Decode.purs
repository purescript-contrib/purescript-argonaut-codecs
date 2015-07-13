module Data.Argonaut.Decode
  ( DecodeJson
  , decodeJson
  , decodeMaybe
  ) where

import Prelude 

import Data.Argonaut.Core
  ( Json()
  , JNumber()
  , JString()
  , foldJsonNull
  , foldJsonBoolean
  , foldJsonNumber
  , foldJsonString
  , foldJsonArray
  , foldJsonObject
  , toArray
  , toNumber
  , toObject
  , toString
  )
import Data.Either (either, Either(..))
import Data.Int (fromNumber)
import Data.Maybe (maybe, Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.String
import Data.List (List(..), toList)
import Control.Alt
import Data.Traversable (traverse)

import qualified Data.StrMap as M
import qualified Data.Map as Map

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a) where
  decodeJson j = (Just <$> decodeJson j) <|> pure Nothing

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f where
    f (Cons a (Cons b Nil)) = Tuple <$> decodeJson a <*> decodeJson b

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson j = (Left <$> decodeJson j) <|> (Right <$> decodeJson j)

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = foldJsonNull (Left "Not null.") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = foldJsonBoolean (Left "Not a Boolean.") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = foldJsonNumber (Left "Not a Number.") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson num = foldJsonNumber (Left "Not a Number.") go num
    where go num = maybe (Left "Not an Int") Right $ fromNumber num 

instance decodeJsonString :: DecodeJson String where
  decodeJson = foldJsonString (Left "Not a String.") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonChar :: DecodeJson Char where
  decodeJson j = (charAt 0 <$> decodeJson j) >>= go where
    go Nothing  = Left $ "Expected character but found: " ++ show j
    go (Just c) = Right c

instance decodeStrMap :: (DecodeJson a) => DecodeJson (M.StrMap a) where
  decodeJson json = maybe (Left "Couldn't decode.") Right $ do
    obj <- toObject json
    traverse decodeMaybe obj

instance decodeArray :: (DecodeJson a) => DecodeJson (Array a) where
  decodeJson json = maybe (Left "Couldn't decode.") Right $ do
    obj <- toArray json
    traverse decodeMaybe obj

instance decodeList :: (DecodeJson a) => DecodeJson (List a) where
  decodeJson json = maybe (Left "Couldn't decode.") Right $ do
    lst <- toList <$> toArray json
    traverse decodeMaybe lst

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map.Map a b) where
  decodeJson j = Map.fromList <$> decodeJson j

decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
decodeMaybe json = either (const Nothing) pure $ decodeJson json
