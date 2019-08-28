module Data.Argonaut.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List(..), (:), toUnfoldable)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Set as S
import Data.String (CodePoint)
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeIdentity :: EncodeJson a => EncodeJson (Identity a) where
  encodeJson (Identity a) = encodeJson a

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson = case _ of
    Nothing -> jsonNull
    Just a -> encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = either (obj "Left") (obj "Right")
    where
    obj :: forall c. EncodeJson c => String -> c -> Json
    obj tag x =
      fromObject 
        $ FO.fromFoldable 
        $ Tuple "tag" (fromString tag) : Tuple "value" (encodeJson x) : Nil

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
  encodeJson = identity

instance encodeJsonCodePoint :: EncodeJson CodePoint where
  encodeJson = encodeJson <<< CP.singleton

instance encodeJsonNonEmpty_Array :: (EncodeJson a) => EncodeJson (NonEmpty Array a) where
  encodeJson (NonEmpty h t) = encodeJson (Arr.cons h t)

instance encodeJsonNonEmptyArray :: (EncodeJson a) => EncodeJson (NonEmptyArray a) where
  encodeJson = encodeJson <<< NEA.toArray

instance encodeJsonNonEmpty_List :: (EncodeJson a) => EncodeJson (NonEmpty List a) where
  encodeJson (NonEmpty h t) = encodeJson (L.insertAt 0 h t)

instance encodeJsonNonEmptyList :: (EncodeJson a) => EncodeJson (NonEmptyList a) where
  encodeJson = encodeJson <<< NEL.toList

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJson <<< CU.singleton

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson = fromArray <<< map encodeJson

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = fromArray <<< map encodeJson <<< toUnfoldable

instance encodeForeignObject :: EncodeJson a => EncodeJson (FO.Object a) where
  encodeJson = fromObject <<< map encodeJson

instance encodeSet :: (Ord a, EncodeJson a) => EncodeJson (S.Set a) where
  encodeJson = encodeJson <<< (S.toUnfoldable :: S.Set a -> List a)

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< (M.toUnfoldable :: M.Map a b -> List (Tuple a b))

instance encodeVoid :: EncodeJson Void where
  encodeJson = absurd

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
