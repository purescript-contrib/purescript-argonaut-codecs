module Data.Argonaut.Decode.Class where

import Prelude (class Ord, Unit, Void, bind, ($), (<<<), (<>))

import Data.Argonaut.Core (Json, toObject)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Data.Argonaut.Decode.Decoders

class DecodeJson a where
  decodeJson :: Decoder a

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
  gDecodeJson object _ =
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName = reflectSymbol sProxy
    in case FO.lookup fieldName object of
      Just jsonVal -> do
        val <- elaborateFailure fieldName <<< decodeJson $ jsonVal

        rest <- gDecodeJson object (RLProxy :: RLProxy tail)

        Right $ Record.insert sProxy val rest

      Nothing ->
        Left $ "JSON was missing expected field: " <> fieldName
