module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log, logShow)

import Data.Argonaut.Core (JObject, Json, isObject, toObject, fromObject, fromArray, fromString, fromNumber, fromBoolean, jsonNull)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Argonaut.Gen (genJson)
import Data.Array (zipWith, nubBy, length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst)

import Test.StrongCheck (SC, quickCheck, quickCheck', (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Data.AlphaNumString (AlphaNumString(..))
import Test.StrongCheck.Gen (Gen, Size, showSample, sized, frequency, oneOf, vectorOf, suchThat, resize)

main :: SC () Unit
main = do
  eitherCheck
  encodeDecodeCheck
  combinatorsCheck

newtype TestJson = TestJson Json

instance arbitraryTestJson :: Arbitrary TestJson where
  arbitrary = TestJson <$> (resize 5 genJson)

encodeDecodeCheck :: SC () Unit
encodeDecodeCheck = do
  log "Testing that any JSON can be encoded and then decoded"
  quickCheck' 20 prop_encode_then_decode

  log "Testing that any JSON can be decoded and then encoded"
  quickCheck' 20 (prop_decode_then_encode)

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
  arbitrary = Obj <$> suchThat (resize 5 genJson) isObject

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
