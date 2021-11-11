module Data.Argonaut.Decode.Class where

import Data.Argonaut.Decode.Decoders

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Prelude (class Ord, Unit, Void, bind, ($), (<$>))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class DecodeJson a where
  decodeJson :: Json -> Either JsonDecodeError a

instance decodeIdentity :: DecodeJson a => DecodeJson (Identity a) where
  decodeJson = decodeIdentity decodeJson

instance decodeJsonMaybe :: DecodeJson a => DecodeJson (Maybe a) where
  decodeJson = decodeMaybe decodeJson

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson = decodeTuple decodeJson decodeJson

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson = decodeEither decodeJson decodeJson

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = decodeNull

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = decodeBoolean

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = decodeNumber

instance decodeJsonInt :: DecodeJson Int where
  decodeJson = decodeInt

instance decodeJsonString :: DecodeJson String where
  decodeJson = decodeString

instance decodeJsonNonEmptyString :: DecodeJson NonEmptyString where
  decodeJson = decodeNonEmptyString

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonNonEmpty_Array :: (DecodeJson a) => DecodeJson (NonEmpty Array a) where
  decodeJson = decodeNonEmpty_Array decodeJson

instance decodeJsonNonEmptyArray :: (DecodeJson a) => DecodeJson (NonEmptyArray a) where
  decodeJson = decodeNonEmptyArray decodeJson

instance decodeJsonNonEmpty_List :: (DecodeJson a) => DecodeJson (NonEmpty List a) where
  decodeJson = decodeNonEmpty_List decodeJson

instance decodeJsonNonEmptyList :: (DecodeJson a) => DecodeJson (NonEmptyList a) where
  decodeJson = decodeNonEmptyList decodeJson

instance decodeJsonCodePoint :: DecodeJson CodePoint where
  decodeJson = decodeCodePoint

instance decodeForeignObject :: DecodeJson a => DecodeJson (FO.Object a) where
  decodeJson = decodeForeignObject decodeJson

instance decodeArray :: DecodeJson a => DecodeJson (Array a) where
  decodeJson = decodeArray decodeJson

instance decodeList :: DecodeJson a => DecodeJson (List a) where
  decodeJson = decodeList decodeJson

instance decodeSet :: (Ord a, DecodeJson a) => DecodeJson (S.Set a) where
  decodeJson = decodeSet decodeJson

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (M.Map a b) where
  decodeJson = decodeMap decodeJson decodeJson

instance decodeVoid :: DecodeJson Void where
  decodeJson = decodeVoid

instance decodeRecord ::
  ( GDecodeJson row list
  , RL.RowToList row list
  ) =>
  DecodeJson (Record row) where
  decodeJson json =
    case toObject json of
      Just object -> gDecodeJson object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

class GDecodeJson (row :: Row Type) (list :: RL.RowList Type) | list -> row where
  gDecodeJson :: forall proxy. FO.Object Json -> proxy list -> Either JsonDecodeError (Record row)

instance gDecodeJsonNil :: GDecodeJson () RL.Nil where
  gDecodeJson _ _ = Right {}

instance gDecodeJsonCons ::
  ( DecodeJsonField value
  , GDecodeJson rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  ) =>
  GDecodeJson row (RL.Cons field value tail) where
  gDecodeJson object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeJsonField fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeJson object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue

class DecodeJsonField a where
  decodeJsonField :: Maybe Json -> Maybe (Either JsonDecodeError a)

instance decodeFieldMaybe ::
  DecodeJson a =>
  DecodeJsonField (Maybe a) where
  decodeJsonField Nothing = Just $ Right Nothing
  decodeJsonField (Just j) = Just $ decodeJson j

else instance decodeFieldId ::
  DecodeJson a =>
  DecodeJsonField a where
  decodeJsonField j = decodeJson <$> j
