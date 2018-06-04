module Test.Main where

import Prelude

import Control.Monad.Gen.Common (genMaybe)
import Data.Argonaut.Core (Json, isObject, stringify, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Gen (genJson)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import Test.QuickCheck (Result(..), quickCheck, (<?>), (===))
import Test.QuickCheck.Gen (Gen, resize, suchThat)

main :: Effect Unit
main = do
  eitherCheck
  encodeDecodeCheck
  combinatorsCheck

genTestJson :: Gen Json
genTestJson = resize 5 genJson

encodeDecodeCheck :: Effect Unit
encodeDecodeCheck = do
  log "Testing that any JSON can be encoded and then decoded"
  quickCheck prop_encode_then_decode

  log "Testing that any JSON can be decoded and then encoded"
  quickCheck prop_decode_then_encode

  where

  prop_encode_then_decode :: Gen Result
  prop_encode_then_decode = do
    json <- genTestJson
    let redecoded = decodeJson (encodeJson json)
    pure $ Right json == redecoded <?> (show (rmap stringify redecoded) <> " /= Right " <> stringify json)

  prop_decode_then_encode :: Gen Result
  prop_decode_then_encode = do
    json <- genTestJson
    let (decoded :: Either String Json) = decodeJson json
    let reencoded = decoded >>= (encodeJson >>> pure)
    pure $ Right json == reencoded <?> (show (rmap stringify reencoded) <> " /= Right " <> stringify json)

genObj :: Gen Json
genObj = suchThat (resize 5 genJson) isObject

combinatorsCheck :: Effect Unit
combinatorsCheck = do
  log "Check assoc builder `:=`"
  quickCheck prop_assoc_builder_str
  log "Check assocOptional builder `:=?`"
  quickCheck prop_assoc_optional_builder_str
  log "Check JAssoc append `~>`"
  quickCheck prop_assoc_append
  log "Check JAssoc appendOptional `~>?`"
  quickCheck prop_assoc_append_optional
  log "Check get field `obj .? 'foo'`"
  quickCheck prop_get_jobject_field

  where

  prop_assoc_builder_str :: Gen Result
  prop_assoc_builder_str = do
    key <- genUnicodeString
    str <- genUnicodeString
    case (key := str) of
      Tuple k json ->
        pure $ Tuple key (decodeJson json) === Tuple k (Right str)

  prop_assoc_optional_builder_str :: Gen Result
  prop_assoc_optional_builder_str = do
    key <- genUnicodeString
    maybeStr <- genMaybe genUnicodeString
    case (key :=? maybeStr) of
      Just (Tuple k json) ->
        pure $ Tuple key (decodeJson json) === Tuple k (Right maybeStr)
      Nothing -> pure Success

  prop_assoc_append :: Gen Result
  prop_assoc_append = do
    key <- genUnicodeString
    val <- genTestJson
    obj <- genObj
    let appended = (key := val) ~> obj
    case toObject appended >>= FO.lookup key of
      Just value -> pure Success
      _ -> pure (Failed "failed to lookup key")

  prop_assoc_append_optional :: Gen Result
  prop_assoc_append_optional = do
    key <- genUnicodeString
    maybeVal <- genMaybe genTestJson
    obj <- genObj
    let appended = (key :=? maybeVal) ~>? obj
    pure case toObject appended >>= FO.lookup key of
      Just value -> isJust maybeVal === true
      _ -> isNothing maybeVal === true

  prop_get_jobject_field :: Gen Result
  prop_get_jobject_field = do
    obj <- genObj
    pure (true === maybe false go (toObject obj))
    where
    go :: FO.Object Json -> Boolean
    go object =
      let keys = FO.keys object
      in foldl (\ok key -> ok && isJust (FO.lookup key object)) true keys

eitherCheck :: Effect Unit
eitherCheck = do
  log "Test EncodeJson/DecodeJson Either instance"
  quickCheck \(x :: Either String String) ->
    case decodeJson (encodeJson x) of
      Right decoded ->
        decoded == x
          <?> ("x = " <> show x <> ", decoded = " <> show decoded)
      Left err ->
        false <?> err
