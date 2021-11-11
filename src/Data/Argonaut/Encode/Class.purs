module Data.Argonaut.Encode.Class where

import Data.Argonaut.Encode.Encoders

import Data.Argonaut.Core (Json, fromObject)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.String.NonEmpty (NonEmptyString)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Prelude (class Ord, Unit, Void, identity, ($))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeIdentity :: EncodeJson a => EncodeJson (Identity a) where
  encodeJson = encodeIdentity encodeJson

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson = encodeMaybe encodeJson

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson = encodeTuple encodeJson encodeJson

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = encodeEither encodeJson encodeJson

instance encodeJsonUnit :: EncodeJson Unit where
  encodeJson = encodeUnit

instance encodeJsonJBoolean :: EncodeJson Boolean where
  encodeJson = encodeBoolean

instance encodeJsonJNumber :: EncodeJson Number where
  encodeJson = encodeNumber

instance encodeJsonInt :: EncodeJson Int where
  encodeJson = encodeInt

instance encodeJsonJString :: EncodeJson String where
  encodeJson = encodeString

instance encodeJsonJson :: EncodeJson Json where
  encodeJson = identity

instance encodeJsonCodePoint :: EncodeJson CodePoint where
  encodeJson = encodeCodePoint

instance encodeNonEmptyString :: EncodeJson NonEmptyString where
  encodeJson = encodeNonEmptyString

instance encodeJsonNonEmpty_Array :: (EncodeJson a) => EncodeJson (NonEmpty Array a) where
  encodeJson = encodeNonEmpty_Array encodeJson

instance encodeJsonNonEmptyArray :: (EncodeJson a) => EncodeJson (NonEmptyArray a) where
  encodeJson = encodeNonEmptyArray encodeJson

instance encodeJsonNonEmpty_List :: (EncodeJson a) => EncodeJson (NonEmpty List a) where
  encodeJson = encodeNonEmpty_List encodeJson

instance encodeJsonNonEmptyList :: (EncodeJson a) => EncodeJson (NonEmptyList a) where
  encodeJson = encodeNonEmptyList encodeJson

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeChar

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson = encodeArray encodeJson

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = encodeList encodeJson

instance encodeForeignObject :: EncodeJson a => EncodeJson (FO.Object a) where
  encodeJson = encodeForeignObject encodeJson

instance encodeSet :: (Ord a, EncodeJson a) => EncodeJson (S.Set a) where
  encodeJson = encodeSet encodeJson

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeMap encodeJson encodeJson

instance encodeVoid :: EncodeJson Void where
  encodeJson = encodeVoid

instance encodeRecord ::
  ( GEncodeJson row list
  , RL.RowToList row list
  ) =>
  EncodeJson (Record row) where
  encodeJson rec = fromObject $ gEncodeJson rec (Proxy :: Proxy list)

class GEncodeJson (row :: Row Type) (list :: RL.RowList Type) where
  gEncodeJson :: forall proxy. Record row -> proxy list -> FO.Object Json

instance gEncodeJsonNil :: GEncodeJson row RL.Nil where
  gEncodeJson _ _ = FO.empty

instance gEncodeJsonCons ::
  ( EncodeJson value
  , GEncodeJson row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  ) =>
  GEncodeJson row (RL.Cons field value tail) where
  gEncodeJson row _ = do
    let _field = Proxy :: Proxy field
    FO.insert
      (reflectSymbol _field)
      (encodeJson $ Record.get _field row)
      (gEncodeJson row (Proxy :: Proxy tail))
