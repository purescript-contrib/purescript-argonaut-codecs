module Data.Argonaut.Decode
  ( DecodeJson
  , decodeJson
  , gDecodeJson
  , gDecodeJson'
  , decodeMaybe
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Data.Argonaut.Core (Json(), foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toNumber, toObject, toString, toBoolean)
import Data.Array (zipWithA)
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.Generic (Generic, GenericSpine(..), GenericSignature(..), fromSpine, toSignature)
import Data.Int (fromNumber)
import Data.List (List(..), toList)
import Data.Map as Map
import Data.Maybe (maybe, Maybe(..))
import Data.String (charAt, toChar)
import Data.StrMap as M
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))

class DecodeJson a where
  decodeJson :: Json -> Either String a

-- | Decode `Json` representation of a value which has a `Generic` type.
gDecodeJson :: forall a. (Generic a) => Json -> Either String a
gDecodeJson json = maybe (Left "fromSpine failed") Right <<< fromSpine
                 =<< gDecodeJson' (toSignature (Proxy :: Proxy a)) json

-- | Decode `Json` representation of a `GenericSpine`.
gDecodeJson' :: GenericSignature -> Json -> Either String GenericSpine
gDecodeJson' signature json = case signature of
  SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
  SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
  SigString -> SString <$> mFail "Expected a string" (toString json)
  SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
  SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
  SigArray thunk -> do
    jArr <- mFail "Expected an array" $ toArray json
    SArray <$> traverse (map const <<< gDecodeJson' (thunk unit)) jArr
  SigRecord props -> do
    jObj <- mFail "Expected an object" $ toObject json
    SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
      pf <- mFail ("'" <> lbl <> "' property missing") (M.lookup lbl jObj)
      sp <- gDecodeJson' (val unit) pf
      pure { recLabel: lbl, recValue: const sp }
  SigProd typeConstr alts -> do
    let decodingErr msg = "When decoding " ++ typeConstr ++ " " ++ msg
    jObj <- mFail (decodingErr "expected an object") (toObject json)
    tag  <- mFail (decodingErr "'tag' string property is missing") (toString =<< M.lookup "tag" jObj)
    case find ((tag ==) <<< _.sigConstructor) alts of
      Nothing -> Left ("'" <> tag <> "' isn't a valid constructor")
      Just { sigValues: sigValues } -> do
        vals <- mFail "'values' array is missing" (toArray =<< M.lookup "values" jObj)
        sps  <- zipWithA (\k -> gDecodeJson' (k unit)) sigValues vals
        pure (SProd tag (const <$> sps))
  where
  mFail :: forall a. String -> Maybe a -> Either String a
  mFail msg = maybe (Left msg) Right

instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a) where
  decodeJson j = (Just <$> decodeJson j) <|> pure Nothing

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f
    where
    f (Cons a (Cons b Nil)) = Tuple <$> decodeJson a <*> decodeJson b
    f _ = Left "Couldn't decode Tuple"

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson j = (Left <$> decodeJson j) <|> (Right <$> decodeJson j)

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = foldJsonNull (Left "Not null") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = foldJsonBoolean (Left "Not a Boolean") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = foldJsonNumber (Left "Not a Number") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson num = foldJsonNumber (Left "Not a Number") go num
    where go num = maybe (Left "Not an Int") Right $ fromNumber num

instance decodeJsonString :: DecodeJson String where
  decodeJson = foldJsonString (Left "Not a String") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonChar :: DecodeJson Char where
  decodeJson j = (charAt 0 <$> decodeJson j) >>= go where
    go Nothing  = Left $ "Expected character but found: " ++ show j
    go (Just c) = Right c

instance decodeStrMap :: (DecodeJson a) => DecodeJson (M.StrMap a) where
  decodeJson json = maybe (Left "Couldn't decode StrMap") Right $ do
    obj <- toObject json
    traverse decodeMaybe obj

instance decodeArray :: (DecodeJson a) => DecodeJson (Array a) where
  decodeJson json = maybe (Left "Couldn't decode Array") Right $ do
    obj <- toArray json
    traverse decodeMaybe obj

instance decodeList :: (DecodeJson a) => DecodeJson (List a) where
  decodeJson json = maybe (Left "Couldn't decode List") Right $ do
    lst <- toList <$> toArray json
    traverse decodeMaybe lst

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map.Map a b) where
  decodeJson j = Map.fromList <$> decodeJson j

decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
decodeMaybe json = either (const Nothing) pure $ decodeJson json
