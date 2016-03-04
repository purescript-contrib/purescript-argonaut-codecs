module Data.Argonaut.Decode
  ( DecodeJson
  , decodeJson
  , gDecodeJson
  , gAesonDecodeJson
  , genericDecodeJson
  , genericDecodeJson'
  , decodeMaybe
  , module Data.Argonaut.Options
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Data.Argonaut.Core (Json(), isNull, foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toNumber, toObject, toString, toBoolean)
import Data.Argonaut.Options
import Data.Array (zipWithA, length)
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.Generic (Generic, GenericSpine(..), GenericSignature(..), DataConstructor(), fromSpine, toSignature)
import Data.Int (fromNumber)
import Data.List (List(..), toList)
import Data.Map as Map
import Data.Maybe (maybe, Maybe(..))
import Data.String (charAt, toChar)
import Data.StrMap as M
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import qualified Data.Array.Unsafe as Unsafe

class DecodeJson a where
  decodeJson :: Json -> Either String a

-- | Decode `Json` representation of a value which has a `Generic` type
-- | with Argonaut options.
gDecodeJson :: forall a. (Generic a) => Json -> Either String a
gDecodeJson = genericDecodeJson argonautOptions

-- | Decode `Json` representation of a value which has a `Generic` type
-- | with Aeson options. Data from Haskell, with Aeson default options can be
-- | decoded with gAesonDecodJson.
gAesonDecodeJson :: forall a. (Generic a) => Json -> Either String a
gAesonDecodeJson = genericDecodeJson aesonOptions

-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a. (Generic a) => Options -> Json -> Either String a
genericDecodeJson opts json = maybe (Left "fromSpine failed") Right <<< fromSpine
                =<< genericDecodeJson' opts (toSignature (Proxy :: Proxy a)) json

-- | Decode `Json` representation of a `GenericSpine`.
genericDecodeJson' :: Options -> GenericSignature -> Json -> Either String GenericSpine
genericDecodeJson' opts signature json = case signature of
 SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
 SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
 SigString -> SString <$> mFail "Expected a string" (toString json)
 SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
 SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
 SigArray thunk -> do
   jArr <- mFail "Expected an array" $ toArray json
   SArray <$> traverse (map const <<< genericDecodeJson' opts (thunk unit)) jArr
 SigRecord props -> do
   jObj <- mFail "Expected an object" $ toObject json
   SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
     pf <- mFail ("'" <> lbl <> "' property missing") (M.lookup lbl jObj)
     sp <- genericDecodeJson' opts (val unit) pf
     pure { recLabel: lbl, recValue: const sp }
 SigProd typeConstr constrSigns -> genericDecodeProdJson' opts typeConstr constrSigns json

genericDecodeProdJson' :: Options ->  String -> Array DataConstructor -> Json -> Either String GenericSpine
genericDecodeProdJson' opts tname constrSigns json =
  if opts.unwrapUnaryRecords && isUnaryRecord constrSigns
  then do
    let constr = Unsafe.head constrSigns
    let unwrapped = Unsafe.head constr.sigValues unit
    r <- genericDecodeJson' opts unwrapped json
    pure (SProd constr.sigConstructor [const r])
  else
    if opts.allNullaryToStringTag && allConstructorsNullary constrSigns
    then decodeFromString
    else decodeTagged
  where
    decodeFromString = do
      tag <- mFail (decodingErr "Constructor name as string expected") (toString json)
      foundConstr <- findConstrFail tag
      pure (SProd foundConstr.sigConstructor [])
    decodeTagged = do
      jObj <- mFail (decodingErr "expected an object") (toObject json)
      tagJson  <- mFail (decodingErr "'" ++ tagL ++ "' property is missing") (M.lookup tagL jObj)
      tag <- mFail (decodingErr "'" ++ tagL ++ "' property is not a string") (toString tagJson)
      foundConstr <-  findConstrFail tag
      jVals <- mFail (decodingErr "'" ++ contL ++ "' property is missing") (M.lookup contL jObj)
      vals <- if opts.flattenContentsArray && (length foundConstr.sigValues == 1)
              then pure [jVals]
              else mFail (decodingErr "Expected array") (toArray jVals)
      sps  <- zipWithA (\k -> genericDecodeJson' opts (k unit)) foundConstr.sigValues vals
      pure (SProd foundConstr.sigConstructor (const <$> sps))

    decodingErr msg = "When decoding a " ++ tname ++ ": " ++ msg
    fixConstr      = opts.constructorTagModifier
    sumConf = case opts.sumEncoding of
      TaggedObject conf -> conf
      _ -> unsafeCrashWith "Only TaggedObject encoding is supported - FIX ME!" -- Not yet supported, waiting for purescript 0.8
    tagL = sumConf.tagFieldName
    contL = sumConf.contentsFieldName
    findConstrFail tag = mFail (decodingErr ("'" <> tag <> "' isn't a valid constructor")) (findConstr tag)
    findConstr tag = find ((tag ==) <<< fixConstr <<< _.sigConstructor) constrSigns

instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a) where
  decodeJson j
    | isNull j = pure Nothing
    | otherwise = (Just <$> decodeJson j) <|> (pure Nothing)

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f
    where
    f (Cons a (Cons b Nil)) = Tuple <$> decodeJson a <*> decodeJson b
    f _ = Left "Couldn't decode Tuple"

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson j =
    case toObject j of
      Just obj -> do
        tag <- just (M.lookup "tag" obj)
        val <- just (M.lookup "value" obj)
        case toString tag of
          Just "Right" ->
            Right <$> decodeJson val
          Just "Left" ->
            Left <$> decodeJson val
          _ ->
            Left "Couldn't decode Either"
      _ ->
        Left "Couldn't decode Either"
    where
    just (Just x) = Right x
    just Nothing  = Left "Couldn't decode Either"

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

mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right
