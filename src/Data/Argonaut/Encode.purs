module Data.Argonaut.Encode
  ( EncodeJson
  , encodeJson
  , gEncodeJson
  , gAesonEncodeJson
  , genericEncodeJson
  , genericEncodeJson'
  , module Data.Argonaut.Options
  ) where

import Prelude

import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Argonaut.Options
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
import Data.Array (length, concatMap, filter, zip, zipWith)
import qualified Data.Array.Unsafe as Unsafe

class EncodeJson a where
  encodeJson :: a -> Json

-- | Encode any `Generic` data structure into `Json`,
-- | formatted according to argonautOptions
gEncodeJson :: forall a. (Generic a) => a -> Json
gEncodeJson = genericEncodeJson argonautOptions

-- | Encode any `Generic` data structure into `Json`,
-- | formatted according to aesonOptions, which is compatible to the default
-- | encoding used by Aeson from Haskell.
gAesonEncodeJson :: forall a. (Generic a) => a -> Json
gAesonEncodeJson = genericEncodeJson aesonOptions

genericEncodeJson :: forall a. (Generic a) => Options -> a -> Json
genericEncodeJson opts = genericEncodeJson' opts sign <<< toSpine
  where sign = toSignature (Proxy :: Proxy a)

-- | Encode `GenericSpine` into `Json`.
genericEncodeJson' :: Options -> GenericSignature -> GenericSpine -> Json
genericEncodeJson' opts sign spine = case spine of
 SInt x            -> fromNumber $ toNumber x
 SString x         -> fromString x
 SChar x           -> fromString $ fromChar x
 SNumber x         -> fromNumber x
 SBoolean x        -> fromBoolean x
 SArray thunks     -> case sign of
                        SigArray elemSign -> fromArray (genericEncodeJson' opts (elemSign unit) <<< (unit #) <$> thunks)
                        --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8
 SProd constr args -> case sign of
                        SigProd _ constrSigns -> genericEncodeProdJson' opts constrSigns constr args
                      --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8
 SRecord fields    -> case sign of
                        SigRecord sigs -> genericEncodeRecordJson' opts sigs fields
                        --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8

genericEncodeRecordJson' :: Options
                        -> Array { recLabel :: String, recValue :: Unit -> GenericSignature }
                        -> Array { recLabel :: String, recValue :: Unit -> GenericSpine }
                        -> Json
genericEncodeRecordJson' opts sigs fields = fromObject <<< foldr (uncurry addField) SM.empty $ zip sigs fields
  where
    addField sig field = SM.insert field.recLabel (genericEncodeJson' opts (sig.recValue unit) (field.recValue unit))

genericEncodeProdJson' :: Options -> Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Json
genericEncodeProdJson' opts constrSigns constr args =
  if opts.unwrapUnaryRecords && isUnaryRecord constrSigns
  then
    genericEncodeJson' opts
      (Unsafe.head (Unsafe.head constrSigns).sigValues unit)
      (Unsafe.head args unit)
  else
    if opts.allNullaryToStringTag && allConstructorsNullary constrSigns
    then fromString fixedConstr
    else fromObject
        $ SM.insert sumConf.tagFieldName (encodeJson fixedConstr)
        $ SM.singleton sumConf.contentsFieldName contents
  where
    sumConf            = case opts. sumEncoding of
                          TaggedObject conf -> conf
    fixedConstr        = opts.constructorTagModifier constr
    encodedArgs        = genericEncodeProdArgs opts constrSigns constr args
    contents           = if opts.flattenContentsArray && length encodedArgs == 1
                         then Unsafe.head encodedArgs
                         else encodeJson encodedArgs



genericEncodeProdArgs :: Options -> Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Array (Json)
genericEncodeProdArgs opts constrSigns constr args = zipWith (genericEncodeJson' opts) sigValues values
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
