module Data.Argonaut.Encode
  ( EncodeJson
  , encodeJson
  , gEncodeJson
  , gEncodeJson'
  ) where

import Prelude

import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Argonaut.Internal
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor(), toSignature)
import Data.Int (toNumber)
import Data.List (List(..), fromList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (fromChar)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (uncurry)
import Data.Array (null, concatMap, filter, zip)

class EncodeJson a where
  encodeJson :: a -> Json

-- | Encode any `Generic` data structure into `Json`.
gEncodeJson :: forall a. (Generic a) => a -> Json
gEncodeJson = gEncodeJson' <<< toSpine

-- | Encode `GenericSpine` into `Json`.
gEncodeJson' :: GenericSpine -> Json
gEncodeJson' spine = case spine of
  SInt x            -> fromNumber $ toNumber x
  SString x         -> fromString x
  SChar x           -> fromString $ fromChar x
  SNumber x         -> fromNumber x
  SBoolean x        -> fromBoolean x
  SArray thunks     -> fromArray (gEncodeJson' <<< (unit #) <$> thunks)
  SProd constr args -> fromObject
                         $ SM.insert    "tag"    (encodeJson constr)
                         $ SM.singleton "values" (encodeJson (gEncodeJson' <<< (unit #) <$> args))
  SRecord fields    -> fromObject $ foldr addField SM.empty fields
    where addField field = SM.insert field.recLabel
                                     (gEncodeJson' $ field.recValue unit)


gAesonEncodeJson :: forall a. (Generic a) => a -> Json
gAesonEncodeJson = gAesonEncodeJson' sign <<< toSpine
  where sign = toSignature (Proxy :: Proxy a)

-- | Encode `GenericSpine` into `Json`.
gAesonEncodeJson' :: GenericSignature -> GenericSpine -> Json
gAesonEncodeJson' sign spine = case spine of
 SInt x            -> fromNumber $ toNumber x
 SString x         -> fromString x
 SChar x           -> fromString $ fromChar x
 SNumber x         -> fromNumber x
 SBoolean x        -> fromBoolean x
 SArray thunks     -> fromArray (gAesonEncodeJson' sign <<< (unit #) <$> thunks)
 SProd constr args -> case sign of
                        SigProd _ constrSigns -> gAesonEncodeProdJson' constrSigns constr args
                      --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8
 SRecord fields    -> case sign of
                        SigRecord sigs -> gAesonEncodeRecordJson' sigs fields
                        --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8

gAesonEncodeRecordJson' :: Array { recLabel :: String, recValue :: Unit -> GenericSignature }
                        -> Array { recLabel :: String, recValue :: Unit -> GenericSpine }
                        -> Json
gAesonEncodeRecordJson' sigs fields = fromObject <<< foldr (uncurry addField) SM.empty $ zip sigs fields
  where
    addField sig field = SM.insert field.recLabel (gAesonEncodeJson' (sig.recValue unit) (field.recValue unit))

gAesonEncodeProdJson' :: Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Json
gAesonEncodeProdJson' constrSigns constr args = fromObject
                                          $ SM.insert "tag" (encodeJson (fixConstr constr))
                                          $ SM.singleton "contents" flattenedArgs
  where
    contents         = if allConstrNullary constrSigns -- If no constructor has any values - serialize as string ...
                       then fromString constr
                       else flattenedArgs
    encodedArgs      = gAesonEncodeProdArgs constrSigns constr args
    flattenedArgs    = case encodedArgs of
                        [a] -> a
                        as  -> encodeJson as

gAesonEncodeProdArgs :: Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Array (Json)
gAesonEncodeProdArgs constrSigns constr args = gAesonEncodeJson' <$> sigValues <*> values
  where
   lSigValues = concatMap (\c -> c.sigValues)
                   <<< filter (\c -> c.sigConstructor == constr) $ constrSigns
   sigValues = (unit #) <$> lSigValues
   values = (unit #) <$> args


instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonNull
  encodeJson (Just a) = encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = either (obj "Left") (obj "Right")
    where
    obj :: forall c. (EncodeJson c) => String -> c -> Json
    obj tag x =
      fromObject $ SM.fromList $
        Cons (Tuple "tag" (fromString tag)) (Cons (Tuple "value" (encodeJson x)) Nil)

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
