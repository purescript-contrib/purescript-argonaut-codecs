module Data.Argonaut.Combinators
  ( (:=)
  , (~>)
  , (?>>=)
  , (.?)
  ) where

import Prelude

import Data.Argonaut.Core (foldJsonObject, fromObject, jsonSingletonObject, Json(), JAssoc(), JObject())
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson, EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))

infix 7 :=
infix 7 .?
infixr 6 ~>
infixl 1 ?>>=

(:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
(:=) k v = Tuple k $ encodeJson v

(~>) :: forall a. (EncodeJson a) => JAssoc -> a -> Json
(~>) (Tuple k v) a = foldJsonObject (jsonSingletonObject k v) (M.insert k v >>> fromObject) (encodeJson a)

(?>>=) :: forall a. Maybe a -> String -> Either String a
(?>>=) (Just x) _   = Right x
(?>>=) _        str = Left $ "Couldn't decode " ++ str

-- obj .? "foo"
(.?) :: forall a. (DecodeJson a) => JObject -> String -> Either String a
(.?) o s = maybe (Left $ "Expected field " ++ show s) decodeJson (M.lookup s o)
