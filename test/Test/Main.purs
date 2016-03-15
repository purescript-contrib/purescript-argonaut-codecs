module Test.Main where

import Prelude

import Data.Argonaut.Core hiding (toNumber)
import Data.Argonaut.Options
import Data.Argonaut.Aeson
import Data.Argonaut.Decode (decodeJson, DecodeJson, genericDecodeJson, genericDecodeJson')
import Data.Argonaut.Encode (encodeJson, EncodeJson, genericEncodeJson, genericEncodeJson')
import Data.Argonaut.Combinators ((:=), (~>), (?>>=))
import Data.Either
import Data.Int (toNumber)
import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Generic
import Data.Foldable (foldl)
import Data.List (toList, List(..))
import Data.StrMap as SM

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Console
import qualified Data.StrMap as M

import Test.Assert (assert', ASSERT)
import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic

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
  return $  let f (AlphaNumString s) = s ++ "x"
                k' = f <$> k
            in  fromObject <<< M.fromList <<< toList <<< nubBy (\a b -> (fst a) == (fst b)) $ zipWith Tuple k' v

genJson :: Size -> Gen Json
genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
genJson n = frequency (Tuple 1.0 genJNull) rest where
  rest = toList [Tuple 2.0 genJBool,
                 Tuple 2.0 genJNumber,
                 Tuple 3.0 genJString,
                 Tuple 1.0 (genJArray n),
                 Tuple 1.0 (genJObject n)]

newtype TestJson = TestJson Json

instance arbitraryTestJson :: Arbitrary TestJson where
  arbitrary = TestJson <$> sized genJson


prop_encode_then_decode :: TestJson -> Boolean
prop_encode_then_decode (TestJson json) =
  Right json == (decodeJson $ encodeJson $ json)

prop_decode_then_encode :: TestJson -> Boolean
prop_decode_then_encode (TestJson json) =
  let decoded = (decodeJson json) :: Either String Json in
  Right json == (decoded >>= (encodeJson >>> pure))

encodeDecodeCheck :: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e ) Unit
encodeDecodeCheck = do
  log "Showing small sample of JSON"
  showSample (genJson 10)

  log "Testing that any JSON can be encoded and then decoded"
  quickCheck' 20 prop_encode_then_decode

  log "Testing that any JSON can be decoded and then encoded"
  quickCheck' 20 prop_decode_then_encode

prop_assoc_builder_str :: Tuple String String -> Boolean
prop_assoc_builder_str (Tuple key str) =
  case (key := str) of
    Tuple k json ->
      (key == k) && (decodeJson json == Right str)

newtype Obj = Obj Json
unObj :: Obj -> Json
unObj (Obj j) = j

instance arbitraryObj :: Arbitrary Obj where
  arbitrary = Obj <$> (genJObject 5)


prop_assoc_append :: (Tuple (Tuple String TestJson) Obj) -> Boolean
prop_assoc_append (Tuple (Tuple key (TestJson val)) (Obj obj)) =
  let appended = assoc ~> obj
      assoc = Tuple key val
  in case toObject appended >>= M.lookup key of
    Just val -> true
    _ -> false


prop_get_jobject_field :: Obj -> Boolean
prop_get_jobject_field (Obj obj) =
  maybe false go $ toObject obj
  where
  go :: JObject -> Boolean
  go obj =
    let keys = M.keys obj
    in foldl (\ok key -> ok && (isJust $ M.lookup key obj)) true keys

assert_maybe_msg :: Boolean
assert_maybe_msg =
  (isLeft (Nothing ?>>= "Nothing is Left"))
  &&
  ((Just 2 ?>>= "Nothing is left") == Right 2)



combinatorsCheck :: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e ) Unit
combinatorsCheck = do
  log "Check assoc builder `:=`"
  quickCheck' 20 prop_assoc_builder_str
  log "Check JAssoc append `~>`"
  quickCheck' 20 prop_assoc_append
  log "Check get field `obj .? 'foo'`"
  quickCheck' 20 prop_get_jobject_field
  log "Assert maybe to either convertion"
  assert assert_maybe_msg

newtype MyRecord = MyRecord { foo :: String, bar :: Int}
derive instance genericMyRecord :: Generic MyRecord

data User = Anonymous
          | Guest String
          | Registered { name :: String
                       , age :: Int
                       , balance :: Number
                       , banned :: Boolean
                       , tweets :: Array String
                       , followers :: Array User
                       }
derive instance genericUser :: Generic User


data AllNullary = Nullary1 | Nullary2 | Nullary3
derive instance genericAllNullary :: Generic AllNullary
instance genericEqAllNullary :: Eq AllNullary where
  eq = gEq

data MultipleArgs = MArgs Int Int String | NArgs
derive instance genericMultipleArgs :: Generic MultipleArgs
instance genericEqMArgs :: Eq MultipleArgs where
  eq = gEq

newtype NewTypeWrapper1 = NewTypeWrapper1 { test :: String }
derive instance genericNewTypeWrapper1 :: Generic NewTypeWrapper1
instance eqNewTypeWrapper1 :: Eq NewTypeWrapper1 where
  eq = gEq
data NewTypeWrapper2 = NewTypeWrapper2 {test :: Int}
derive instance genericNewTypeWrapper2 :: Generic NewTypeWrapper2
instance eqNewTypeWrapper2 :: Eq NewTypeWrapper2 where
  eq = gEq

prop_iso_generic :: Options -> GenericValue -> Boolean
prop_iso_generic opts genericValue =
  Right val.spine == genericDecodeJson' opts val.signature (genericEncodeJson' opts val.signature val.spine)
  where val = runGenericValue genericValue

prop_decoded_spine_valid :: Options -> GenericValue -> Boolean
prop_decoded_spine_valid opts genericValue =
  Right true == (isValidSpine val.signature <$> genericDecodeJson' opts val.signature (genericEncodeJson' opts val.signature val.spine))
  where val = runGenericValue genericValue

checkAesonCompat :: Boolean
checkAesonCompat =
  let
    myTuple = Tuple (Tuple 1 2) "Hello"
    myJust = Just "Test"
    myNothing = Nothing :: Maybe Int
    myLeft = Left "Foo" :: Either String String
    myRight = Right "Bar" :: Either Int String
  in
        gAesonEncodeJson myTuple   == fromArray [fromNumber $ toNumber 1, fromNumber $ toNumber 2, fromString "Hello"]
    &&  gAesonEncodeJson myJust    == fromString "Test"
    &&  gAesonEncodeJson myNothing == jsonNull
    &&  gAesonEncodeJson myLeft    == fromObject (SM.fromList (Tuple "Left" (fromString "Foo") `Cons` Nil))
    &&  gAesonEncodeJson myRight   == fromObject (SM.fromList (Tuple "Right" (fromString "Bar") `Cons` Nil))


genericsCheck :: forall e. Options -> Eff ( err :: EXCEPTION , random :: RANDOM , console :: CONSOLE, assert :: ASSERT | e) Unit
genericsCheck opts= do
  let vNullary = Nullary2
  let mArgs = MArgs 9 20 "Hello"
  let ntw1 = NewTypeWrapper1 { test : "hello" }
  let ntw2 = NewTypeWrapper2 { test : 9 }
  let mJust = Just "Test"
  let mNothing = Nothing :: Maybe Int
  let mRight = Right 9 :: Either String Int
  let mLeft = Right (Left 2) :: Either String (Either Int Int)
  let mTuple = Tuple (Tuple (Tuple 2 3) "haha") "test"
  log "Check that decodeJson' and encodeJson' form an isomorphism .."
  assert' " Check all nullary:" (valEncodeDecode opts vNullary)
  assert' " Check multiple args:" (valEncodeDecode opts mArgs)
  assert' " Check new type wrapper (1) encoding:" (valEncodeDecode opts ntw1)
  assert' " Check new type wrapper (2) encoding:" (valEncodeDecode opts ntw2)
  assert' " Check Just" (valEncodeDecode opts mJust)
  assert' " Check Nothing" (valEncodeDecode opts mNothing)
  assert' " Check Right" (valEncodeDecode opts mRight)
  assert' " Check Left" (valEncodeDecode opts mLeft)
  assert' " Check tuple" (valEncodeDecode opts mTuple)

  quickCheck (prop_iso_generic opts)
  log "Check that decodeJson' returns a valid spine"
  quickCheck (prop_decoded_spine_valid opts)
  log "Print samples of values encoded with genericEncodeJson"
  print $ genericEncodeJson opts 5
  print $ genericEncodeJson opts [1, 2, 3, 5]
  print $ genericEncodeJson opts (Just "foo")
  print $ genericEncodeJson opts (Right "foo" :: Either String String)
  print $ genericEncodeJson opts $ MyRecord { foo: "foo", bar: 2}
  print $ genericEncodeJson opts "foo"
  print $ genericEncodeJson opts Anonymous
  print $ genericEncodeJson opts $ Guest "guest's handle"
  print $ genericEncodeJson opts $ Registered { name: "user1"
                                   , age: 5
                                   , balance: 26.6
                                   , banned: false
                                   , tweets: ["Hello", "What's up"]
                                   , followers: [ Anonymous
                                                , Guest "someGuest"
                                                , Registered { name: "user2"
                                                             , age: 6
                                                             , balance: 32.1
                                                             , banned: false
                                                             , tweets: ["Hi"]
                                                             , followers: []
                                                             }]}
  print $ genericEncodeJson opts Nullary1
  print $ genericEncodeJson opts Nullary2
  print $ genericEncodeJson opts $ MArgs 9 22 "Test"
  print $ genericEncodeJson opts NArgs
  print $ genericEncodeJson opts ntw1
  print $ genericEncodeJson opts ntw2

  where
    valEncodeDecode :: forall a. (Eq a, Generic a) => Options ->  a -> Boolean
    valEncodeDecode opts val = ((Right val) ==) <<< genericDecodeJson opts <<< genericEncodeJson opts $ val

eitherCheck :: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e ) Unit
eitherCheck = do
  log "Test EncodeJson/DecodeJson Either instance"
  quickCheck \(x :: Either String String) ->
    case decodeJson (encodeJson x) of
      Right decoded ->
        decoded == x
          <?> ("x = " <> show x <> ", decoded = " <> show decoded)
      Left err ->
        false <?> err

main:: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE, assert :: ASSERT | e ) Unit
main = do
  eitherCheck
  encodeDecodeCheck
  combinatorsCheck
  assert' "aesonCompatcheck: " checkAesonCompat
  log "genericsCheck check for argonautOptions"
  genericsCheck argonautOptions
  log "genericsCheck check for aesonOptions"
  genericsCheck aesonOptions
  log "genericsCheck check for unwrapUnaryOptions"
  let unwrapOpts = case aesonOptions of Options a -> a
  let unwrapUnaryOptions = Options $ unwrapOpts { unwrapUnaryRecords = true }
  genericsCheck unwrapUnaryOptions
