module Test.Main where

import Prelude

import Data.Argonaut.Core 
import Data.Argonaut.Decode (decodeJson, DecodeJson)
import Data.Argonaut.Encode (encodeJson, EncodeJson)
import Data.Argonaut.Combinators ((:=), (~>), (?>>=), (.?))
import Data.Either
import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Foldable (foldl)
import Data.List (toList, List(..))
import Control.Monad.Eff.Console
import qualified Data.StrMap as M

import Test.StrongCheck
import Test.StrongCheck.Gen


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

-- orphan, but it's just for tests 
instance arbitraryJson :: Arbitrary Json where
  arbitrary = sized genJson


prop_encode_then_decode :: Json -> Boolean
prop_encode_then_decode json =
  Right json == (decodeJson $ encodeJson $ json)

prop_decode_then_encode :: Json -> Boolean
prop_decode_then_encode json =
  let decoded = (decodeJson json) :: Either String Json in
  Right json == (decoded >>= (encodeJson >>> pure))


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


prop_assoc_append :: (Tuple JAssoc Obj) -> Boolean
prop_assoc_append (Tuple assoc@(Tuple key val) (Obj obj)) =
  let appended = assoc ~> obj
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




combinatorsCheck = do
  log "Check assoc builder `:=`"
  quickCheck' 20 prop_assoc_builder_str
  log "Check JAssoc append `~>`"
  quickCheck' 20 prop_assoc_append
  log "Check get field `obj .? 'foo'`"
  quickCheck' 20 prop_get_jobject_field
  log "Assert maybe to either convertion"
  assert assert_maybe_msg
  


main = do
  encodeDecodeCheck
  combinatorsCheck
