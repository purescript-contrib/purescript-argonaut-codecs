module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core (JObject, Json, isObject, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Gen (genJson)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Test.StrongCheck (SC, quickCheck, quickCheck', (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (suchThat, resize)

main :: SC () Unit
main = do
  eitherCheck
  encodeDecodeCheck
  combinatorsCheck

newtype TestJson = TestJson Json

instance encodeJsonTestJson :: EncodeJson TestJson where
  encodeJson (TestJson x) = encodeJson x

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
  log "Check assocOptional builder `:=?`"
  quickCheck' 20 prop_assoc_optional_builder_str
  log "Check JAssoc append `~>`"
  quickCheck' 20 prop_assoc_append
  log "Check JAssoc appendOptional `~>?`"
  quickCheck' 20 prop_assoc_append_optional
  log "Check get field `obj .? 'foo'`"
  quickCheck' 20 prop_get_jobject_field

  where

  prop_assoc_builder_str :: Tuple String String -> Boolean
  prop_assoc_builder_str (Tuple key str) =
    case (key := str) of
      Tuple k json ->
        (key == k) && (decodeJson json == Right str)

  prop_assoc_optional_builder_str :: Tuple String (Maybe String) -> Boolean
  prop_assoc_optional_builder_str (Tuple key maybeStr) =
    case (key :=? maybeStr) of
      Just (Tuple k json) ->
        (key == k) && (decodeJson json == Right maybeStr)
      Nothing -> true

  prop_assoc_append :: (Tuple (Tuple String TestJson) Obj) -> Boolean
  prop_assoc_append (Tuple (Tuple key (TestJson val)) (Obj obj)) =
    let appended = (key := val) ~> obj
    in case toObject appended >>= SM.lookup key of
      Just value -> true
      _ -> false

  prop_assoc_append_optional :: Tuple (Tuple String (Maybe TestJson)) Obj -> Boolean
  prop_assoc_append_optional (Tuple (Tuple key maybeVal) (Obj obj)) =
    let appended = (key :=? maybeVal) ~>? obj
    in case toObject appended >>= SM.lookup key of
      Just value -> isJust maybeVal 
      _ -> isNothing maybeVal 

  prop_get_jobject_field :: Obj -> Boolean
  prop_get_jobject_field (Obj obj) =
    maybe false go $ toObject obj
    where
    go :: JObject -> Boolean
    go object =
      let keys = SM.keys object
      in foldl (\ok key -> ok && isJust (SM.lookup key object)) true keys

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
