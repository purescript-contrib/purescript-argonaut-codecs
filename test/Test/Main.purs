module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log, logShow)

import Data.Argonaut.Core (JObject, Json, toObject, fromObject, fromArray, fromString, fromNumber, fromBoolean, jsonNull)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson, gEncodeJson, (:=), (~>))
import Data.Array (zipWith, nubBy, length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Generic (class Generic)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst)

import Test.StrongCheck (SC, quickCheck, quickCheck', (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Data.AlphaNumString (AlphaNumString(..))
import Test.StrongCheck.Gen (Gen, Size, showSample, sized, frequency, oneOf, vectorOf)

main :: SC () Unit
main = do
  eitherCheck
  encodeDecodeCheck
  combinatorsCheck
  genericsCheck

genJNull :: Gen Json
genJNull = pure jsonNull

genJBool :: Gen Json
genJBool = fromBoolean <$> arbitrary

genJNumber :: Gen Json
genJNumber = fromNumber <$> arbitrary

genJString :: Gen Json
genJString = fromString <$> arbitrary

genJArray :: Size -> Gen Json
genJArray sz = fromArray <$> vectorOf sz (genJson $ sz - 1)

genJObject :: Size -> Gen Json
genJObject sz = do
  v <- vectorOf sz (genJson $ sz - 1)
  k <- vectorOf (length v) (arbitrary :: Gen AlphaNumString)
  pure
    let
      f (AlphaNumString s) = s <> "x"
      k' = f <$> k
    in
      fromObject <<< SM.fromFoldable <<< nubBy (eq `on` fst) $ zipWith Tuple k' v

genJson :: Size -> Gen Json
genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
genJson n = frequency (Tuple 1.0 genJNull) rest where
  rest = fromFoldable
    [ Tuple 2.0 genJBool
    , Tuple 2.0 genJNumber
    , Tuple 3.0 genJString
    , Tuple 1.0 (genJArray n)
    , Tuple 1.0 (genJObject n)
    ]

newtype TestJson = TestJson Json

instance arbitraryTestJson :: Arbitrary TestJson where
  arbitrary = TestJson <$> sized genJson

encodeDecodeCheck :: SC () Unit
encodeDecodeCheck = do
  log "Showing small sample of JSON"
  showSample (genJson 10)

  log "Testing that any JSON can be encoded and then decoded"
  quickCheck' 20 prop_encode_then_decode

  log "Testing that any JSON can be decoded and then encoded"
  quickCheck' 20 prop_decode_then_encode

  where

  prop_encode_then_decode :: TestJson -> Boolean
  prop_encode_then_decode (TestJson json) =
    Right json == decodeJson (encodeJson json)

  prop_decode_then_encode :: TestJson -> Boolean
  prop_decode_then_encode (TestJson json) =
    let decoded = (decodeJson json) :: Either String Json in
    Right json == (decoded >>= (encodeJson >>> pure))

newtype Obj = Obj Json
unObj :: Obj -> Json
unObj (Obj j) = j

instance arbitraryObj :: Arbitrary Obj where
  arbitrary = Obj <$> genJObject 5

combinatorsCheck :: SC () Unit
combinatorsCheck = do
  log "Check assoc builder `:=`"
  quickCheck' 20 prop_assoc_builder_str
  log "Check JAssoc append `~>`"
  quickCheck' 20 prop_assoc_append
  log "Check get field `obj .? 'foo'`"
  quickCheck' 20 prop_get_jobject_field

  where

  prop_assoc_builder_str :: Tuple String String -> Boolean
  prop_assoc_builder_str (Tuple key str) =
    case (key := str) of
      Tuple k json ->
        (key == k) && (decodeJson json == Right str)

  prop_assoc_append :: (Tuple (Tuple String TestJson) Obj) -> Boolean
  prop_assoc_append (Tuple (Tuple key (TestJson val)) (Obj obj)) =
    let appended = (key := val) ~> obj
    in case toObject appended >>= SM.lookup key of
      Just val -> true
      _ -> false

  prop_get_jobject_field :: Obj -> Boolean
  prop_get_jobject_field (Obj obj) =
    maybe false go $ toObject obj
    where
    go :: JObject -> Boolean
    go obj =
      let keys = SM.keys obj
      in foldl (\ok key -> ok && isJust (SM.lookup key obj)) true keys

newtype MyRecord = MyRecord { foo :: String, bar :: Int }

derive instance genericMyRecord :: Generic MyRecord

data User
  = Anonymous
  | Guest String
  | Registered
      { name :: String
      , bio :: Maybe String
      , age :: Int
      , balance :: Number
      , banned :: Boolean
      , tweets :: Array String
      , followers :: Array User
      }

derive instance genericUser :: Generic User

genericsCheck :: SC () Unit
genericsCheck = do
  log "Print samples of values encoded with gEncodeJson"
  logShow $ gEncodeJson 5
  logShow $ gEncodeJson [1, 2, 3, 5]
  logShow $ gEncodeJson (Just "foo")
  logShow $ gEncodeJson (Right "foo" :: Either String String)
  logShow $ gEncodeJson $ MyRecord { foo: "foo", bar: 2}
  logShow $ gEncodeJson "foo"
  logShow $ gEncodeJson Anonymous
  logShow $ gEncodeJson $ Guest "guest's handle"
  logShow $ gEncodeJson $ Registered
    { name: "user1"
    , bio: Just "Ordinary User"
    , age: 5
    , balance: 26.6
    , banned: false
    , tweets: ["Hello", "What's up"]
    , followers:
        [ Anonymous
        , Guest "someGuest"
        , Registered
            { name: "user2"
            , bio: Nothing
            , age: 6
            , balance: 32.1
            , banned: false
            , tweets: ["Hi"]
            , followers: []
            }
        ]
    }

eitherCheck :: SC () Unit
eitherCheck = do
  log "Test EncodeJson/DecodeJson Either instance"
  quickCheck \(x :: Either String String) ->
    case decodeJson (encodeJson x) of
      Right decoded ->
        decoded == x
          <?> ("x = " <> show x <> ", decoded = " <> show decoded)
      Left err ->
        false <?> err
