module Test.Main where

import Prelude

import Control.Monad.Gen.Common (genMaybe)
import Data.Argonaut.Core (Json, isObject, stringify, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Gen (genJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as FO
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, resize, suchThat)
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)


main :: Effect Unit
main = runTest do
  suite "Either Check" eitherCheck
  suite "Encode/Decode Checks" encodeDecodeCheck
  suite "Encode/Decode Record Checks" encodeDecodeRecordCheck
  suite "Combinators Checks" combinatorsCheck
  suite "Manual Combinators Checks" manualCombinatorsCheck
  suite "Error Message Checks" errorMsgCheck


genTestRecord
  :: Gen (Record
       ( i :: Int
       , n :: Number
       , s :: String
       ))
genTestRecord = arbitrary

encodeDecodeRecordCheck :: TestSuite
encodeDecodeRecordCheck = do
  test "Testing that any record can be encoded and then decoded" do
    quickCheck rec_encode_then_decode

  where
   rec_encode_then_decode :: Gen Result
   rec_encode_then_decode = do
    rec <- genTestRecord
    let redecoded = decodeJson (encodeJson rec)
    pure $ Right rec == redecoded <?> (show redecoded <> " /= Right " <> show rec)


genTestJson :: Gen Json
genTestJson = resize 5 genJson

encodeDecodeCheck :: TestSuite
encodeDecodeCheck = do
  test "Testing that any JSON can be encoded and then decoded" do
    quickCheck prop_encode_then_decode

  test "Testing that any JSON can be decoded and then encoded" do
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

combinatorsCheck :: TestSuite
combinatorsCheck = do
  test "Check assoc builder `:=`" do
    quickCheck prop_assoc_builder_str
  test "Check assocOptional builder `:=?`" do
    quickCheck prop_assoc_optional_builder_str
  test "Check JAssoc append `~>`" do
    quickCheck prop_assoc_append
  test "Check JAssoc appendOptional `~>?`" do
    quickCheck prop_assoc_append_optional
  test "Check get field `obj .: 'foo'`" do -- this doesn't really test .:
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

eitherCheck :: TestSuite
eitherCheck = do
  test "Test EncodeJson/DecodeJson Either test" do
    quickCheck \(x :: Either String String) ->
      case decodeJson (encodeJson x) of
        Right decoded ->
          decoded == x
            <?> ("x = " <> show x <> ", decoded = " <> show decoded)
        Left err ->
          false <?> err

manualCombinatorsCheck :: TestSuite
manualCombinatorsCheck = do
  test "Test that decoding custom record is successful" do
    case decodeJson =<< jsonParser fooJson of
      Right (Foo _) -> success
      Left err -> failure err
  suite "Test decoding empty record" testEmptyCases
  suite "Test decoding missing 'bar' key" testBarCases
  suite "Test decoding missing 'baz' key" testBazCases
  suite "Test decoding with all fields present" testFullCases
  where
  testEmptyCases :: TestSuite
  testEmptyCases = do
    test "Empty Json should decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedEmptyJson of
        Right (FooNested { bar: Nothing, baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedEmptyJson)
    test "Json with null values should fail to decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedEmptyJsonNull of
        Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> fooNestedEmptyJsonNull)
        _ -> success
    test "Empty Json should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedEmptyJson of
        Right (FooNested' { bar: Nothing, baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedEmptyJson)
    test "Json with null values should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedEmptyJsonNull of
        Right (FooNested' { bar: Nothing, baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedEmptyJsonNull)

  testBarCases :: TestSuite
  testBarCases = do
    test "Missing 'bar' key should decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedBazJson of
        Right (FooNested { bar: Nothing, baz: true }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBazJson)
    test "Null 'bar' key should fail to decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedBazJsonNull of
        Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> fooNestedBazJsonNull)
        _ -> success
    test "Missing 'bar' key should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedBazJson of
        Right (FooNested' { bar: Nothing, baz: true }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBazJson)
    test "Null 'bar' key should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedBazJsonNull of
        Right (FooNested' { bar: Nothing, baz: true }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBazJsonNull)

  testBazCases :: TestSuite
  testBazCases = do
    test "Missing 'baz' key should decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedBarJson of
        Right (FooNested { bar: Just [1], baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBarJson)
    test "Null 'baz' key should fail to decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedBarJsonNull of
        Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> fooNestedBarJsonNull)
        _ -> success
    test "Missing 'baz' key should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedBarJson of
        Right (FooNested' { bar: Just [1], baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBarJson)
    test "Null 'baz' key should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedBarJsonNull of
        Right (FooNested' { bar: Just [1], baz: false }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedBarJsonNull)

  testFullCases :: TestSuite
  testFullCases = do
    test "Json should decode to FooNested" do
      case decodeJson =<< jsonParser fooNestedFullJson of
        Right (FooNested { bar: Just [1], baz: true }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedFullJson)
    test "Json should decode to FooNested'" do
      case decodeJson =<< jsonParser fooNestedFullJson of
        Right (FooNested { bar: Just [1], baz: true }) -> success
        _ -> failure ("Failed to properly decode JSON string: " <> fooNestedFullJson)

  fooJson :: String
  fooJson = """{ "bar": [1, 2, 3], "baz": true }"""

  fooNestedEmptyJson :: String
  fooNestedEmptyJson = "{ }"

  fooNestedEmptyJsonNull :: String
  fooNestedEmptyJsonNull = """{ "bar": null, "baz": null }"""

  fooNestedBazJson :: String
  fooNestedBazJson = """{ "baz": true }"""

  fooNestedBazJsonNull :: String
  fooNestedBazJsonNull = """{ "bar": null, "baz": true }"""

  fooNestedBarJson :: String
  fooNestedBarJson = """{ "bar": [1] }"""

  fooNestedBarJsonNull :: String
  fooNestedBarJsonNull = """{ "bar": [1], "baz": null }"""

  fooNestedFullJson :: String
  fooNestedFullJson = """{ "bar": [1], "baz": true }"""

errorMsgCheck :: TestSuite
errorMsgCheck = do
  test "Test that decoding array fails with the proper message" do
    case notBar of
      Left err -> Assert.equal barErr err
      _ -> failure "Should have failed to decode"
  test "Test that decoding record fails with the proper message" do
    case notBaz of
      Left err -> Assert.equal bazErr err
      _ -> failure "Should have failed to decode"
  where
  barErr :: String
  barErr =
    "Failed to decode key 'bar': "
    <> "Couldn't decode Array (Failed at index 1): "
    <> "Value is not a Number"

  bazErr :: String
  bazErr =
    "Failed to decode key 'baz': "
    <> "Value is not a Boolean"

  notBar :: Either String Foo
  notBar = decodeJson =<< jsonParser """{ "bar": [1, true, 3], "baz": false }"""

  notBaz :: Either String Foo
  notBaz = decodeJson =<< jsonParser """{ "bar": [1, 2, 3], "baz": 42 }"""

newtype Foo = Foo
  { bar :: Array Int
  , baz :: Boolean
  }

instance decodeJsonFoo :: DecodeJson Foo where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .: "bar"
    baz <- x .: "baz"
    pure $ Foo { bar, baz }

newtype FooNested = FooNested
  { bar :: Maybe (Array Int)
  , baz :: Boolean
  }

instance decodeJsonFooNested :: DecodeJson FooNested where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .:! "bar"
    baz <- x .:! "baz" .!= false
    pure $ FooNested { bar, baz }

newtype FooNested' = FooNested'
  { bar :: Maybe (Array Int)
  , baz :: Boolean
  }

instance decodeJsonFooNested' :: DecodeJson FooNested' where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .:? "bar"
    baz <- x .:? "baz" .!= false
    pure $ FooNested' { bar, baz }
