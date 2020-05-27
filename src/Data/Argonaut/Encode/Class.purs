module Data.Argonaut.Encode.Class where

import Prelude (class Ord, Unit, Void, ($))

import Data.Argonaut.Core (Json, fromObject)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe)
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
import Data.Argonaut.Encode.Encoders

class EncodeJson a where
  encodeJson :: Encoder a

instance encodeIdentity :: EncodeJson a => EncodeJson (Identity a) where
  encodeJson = encodeIdentity encodeJson

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson = encodeJsonMaybe encodeJson

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson = encodeJsonTuple encodeJson encodeJson

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = encodeJsonEither encodeJson encodeJson

instance encodeJsonUnit :: EncodeJson Unit where
  encodeJson = encodeJsonUnit

instance encodeJsonJBoolean :: EncodeJson Boolean where
  encodeJson = encodeJsonJBoolean

instance encodeJsonJNumber :: EncodeJson Number where
  encodeJson = encodeJsonJNumber

instance encodeJsonInt :: EncodeJson Int where
  encodeJson = encodeJsonInt

instance encodeJsonJString :: EncodeJson String where
  encodeJson = encodeJsonJString

instance encodeJsonJson :: EncodeJson Json where
  encodeJson = encodeJsonJson

instance encodeJsonCodePoint :: EncodeJson CodePoint where
  encodeJson = encodeJsonCodePoint

instance encodeJsonNonEmpty_Array :: (EncodeJson a) => EncodeJson (NonEmpty Array a) where
  encodeJson = encodeJsonNonEmpty_Array encodeJson

instance encodeJsonNonEmptyArray :: (EncodeJson a) => EncodeJson (NonEmptyArray a) where
  encodeJson = encodeJsonNonEmptyArray encodeJson

instance encodeJsonNonEmpty_List :: (EncodeJson a) => EncodeJson (NonEmpty List a) where
  encodeJson = encodeJsonNonEmpty_List encodeJson

instance encodeJsonNonEmptyList :: (EncodeJson a) => EncodeJson (NonEmptyList a) where
  encodeJson = encodeJsonNonEmptyList encodeJson

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJsonChar

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson = encodeJsonArray encodeJson

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = encodeJsonList encodeJson

instance encodeForeignObject :: EncodeJson a => EncodeJson (FO.Object a) where
  encodeJson = encodeForeignObject encodeJson

instance encodeSet :: (Ord a, EncodeJson a) => EncodeJson (S.Set a) where
  encodeJson = encodeSet encodeJson

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeMap encodeJson encodeJson

instance encodeVoid :: EncodeJson Void where
  encodeJson = encodeVoid

instance encodeRecord
  :: ( GEncodeJson row list
     , RL.RowToList row list
     )
  => EncodeJson (Record row) where
  encodeJson rec = fromObject $ gEncodeJson rec (RLProxy :: RLProxy list)

class GEncodeJson (row :: # Type) (list :: RL.RowList) where
  gEncodeJson :: Record row -> RLProxy list -> FO.Object Json

instance gEncodeJsonNil :: GEncodeJson row RL.Nil where
  gEncodeJson _ _ = FO.empty

instance gEncodeJsonCons
  :: ( EncodeJson value
     , GEncodeJson row tail
     , IsSymbol field
     , Row.Cons field value tail' row
     )
  => GEncodeJson row (RL.Cons field value tail) where
  gEncodeJson row _ = do
    let sProxy = SProxy :: SProxy field
    FO.insert
      (reflectSymbol sProxy)
      (encodeJson $ Record.get sProxy row)
      (gEncodeJson row (RLProxy :: RLProxy tail))
