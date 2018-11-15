module Data.Argonaut.Decode.Class where

import Prelude

import Control.Alternative (class Plus)
import Data.Argonaut.Core (Json, isNull, caseJsonNull, caseJsonBoolean, caseJsonNumber, caseJsonString, toArray, toObject, toString, stringify)
import Data.Array as Arr
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), note)
import Data.Int (fromNumber)
import Data.List (List(..), (:), fromFoldable)
import Data.List as L
import Data.Map as M
import Data.Maybe (maybe, Maybe(..))
import Data.NonEmpty (NonEmpty, singleton, (:|))
import Data.String (CodePoint, codePointAt)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

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
        tag <- maybe (Left "Expected field 'tag'") Right $ FO.lookup "tag" obj
        val <- maybe (Left "Expected field 'value'") Right $ FO.lookup "value" obj
        case toString tag of
          Just "Right" -> Right <$> decodeJson val
          Just "Left" -> Left <$> decodeJson val
          _ -> Left "'tag' field was not \"Left\" or \"Right\""

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = caseJsonNull (Left "Value is not a null") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = caseJsonBoolean (Left "Value is not a Boolean") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = caseJsonNumber (Left "Value is not a Number") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson
    = maybe (Left "Value is not an integer") Right
    <<< fromNumber
    <=< decodeJson

instance decodeJsonString :: DecodeJson String where
  decodeJson = caseJsonString (Left "Value is not a String") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonNonEmptyArray :: (DecodeJson a) => DecodeJson (NonEmpty Array a) where
  decodeJson
    = lmap ("Couldn't decode NonEmpty Array: " <> _)
    <<< (traverse decodeJson <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> x.head :| x.tail) <<< note " is empty" <<< Arr.uncons) <=< decodeJArray)

instance decodeJsonNonEmptyList :: (DecodeJson a) => DecodeJson (NonEmpty List a) where
  decodeJson
    = lmap ("Couldn't decode NonEmpty List: " <> _)
    <<< (traverse decodeJson <=< (lmap ("JSON Array" <> _) <<< rmap (\x -> x.head :| x.tail) <<< note " is empty" <<< L.uncons) <=< map (map fromFoldable) decodeJArray)

instance decodeJsonChar :: DecodeJson CodePoint where
  decodeJson j =
    maybe (Left $ "Expected character but found: " <> stringify j) Right
      =<< codePointAt 0 <$> decodeJson j

instance decodeForeignObject :: DecodeJson a => DecodeJson (FO.Object a) where
  decodeJson
    = lmap ("Couldn't decode ForeignObject: " <> _)
    <<< (traverse decodeJson <=< decodeJObject)

instance decodeArray :: DecodeJson a => DecodeJson (Array a) where
  decodeJson
    = lmap ("Couldn't decode Array (" <> _)
    <<< (traverseWithIndex f <=< decodeJArray)
    where
      msg i m = "Failed at index " <> show i <> "): " <> m
      f i = lmap (msg i) <<< decodeJson

instance decodeList :: DecodeJson a => DecodeJson (List a) where
  decodeJson
    = lmap ("Couldn't decode List: " <> _)
    <<< (traverse decodeJson <=< map (map fromFoldable) decodeJArray)

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (M.Map a b) where
  decodeJson = map (M.fromFoldable :: List (Tuple a b) -> M.Map a b) <<< decodeJson

instance decodeVoid :: DecodeJson Void where
  decodeJson _ = Left "Value cannot be Void"

decodeJArray :: Json -> Either String (Array Json)
decodeJArray = maybe (Left "Value is not an Array") Right <<< toArray

decodeJObject :: Json -> Either String (FO.Object Json)
decodeJObject = maybe (Left "Value is not an Object") Right <<< toObject

instance decodeRecord
  :: ( GDecodeJson row list
     , RL.RowToList row list
     )
  => DecodeJson (Record row) where

  decodeJson json =
    case toObject json of
      Just object -> gDecodeJson object (RLProxy :: RLProxy list)
      Nothing     -> Left "Could not convert JSON to object"

class GDecodeJson (row :: # Type) (list :: RL.RowList) | list -> row where
  gDecodeJson :: FO.Object Json -> RLProxy list -> Either String (Record row)

instance gDecodeJsonNil :: GDecodeJson () RL.Nil where
  gDecodeJson _ _ = Right {}

instance gDecodeJsonCons
  :: ( DecodeJson value
     , GDecodeJson rowTail tail
     , IsSymbol field
     , Row.Cons field value rowTail row
     , Row.Lacks field rowTail
     )
  => GDecodeJson row (RL.Cons field value tail) where

  gDecodeJson object _ = do
    let sProxy :: SProxy field
        sProxy = SProxy

        fieldName = reflectSymbol sProxy

    rest <- gDecodeJson object (RLProxy :: RLProxy tail)

    case FO.lookup fieldName object of
      Just jsonVal -> do
        val <- decodeJson jsonVal
        Right $ Record.insert sProxy val rest

      Nothing ->
        Left $ "JSON was missing expected field: " <> fieldName
