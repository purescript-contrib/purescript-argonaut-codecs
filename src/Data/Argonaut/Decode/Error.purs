-- | Originally implemented in:
-- | https://github.com/garyb/purescript-codec-argonaut
module Data.Argonaut.Decode.Error where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Generic.Rep (class Generic)

-- | Error type for failures while decoding.
data JsonDecodeError' a
  = TypeMismatch String
  | UnexpectedValue Json
  | AtIndex Int (JsonDecodeError' a)
  | AtKey String (JsonDecodeError' a)
  | Named String (JsonDecodeError' a)
  | MissingValue
  | Custom a

type JsonDecodeError = JsonDecodeError' Void

derive instance eqJsonDecodeError :: Eq a => Eq (JsonDecodeError' a)
derive instance ordJsonDecodeError :: Ord a => Ord (JsonDecodeError' a)
derive instance genericJsonDecodeError :: Generic (JsonDecodeError' a) _

instance showJsonDecodeError :: Show a => Show (JsonDecodeError' a) where
  show = case _ of
    TypeMismatch s -> "(TypeMismatch " <> show s <> ")"
    UnexpectedValue j -> "(UnexpectedValue " <> stringify j <> ")"
    AtIndex i e -> "(AtIndex " <> show i <> " " <> show e <> ")"
    AtKey k e -> "(AtKey " <> show k <> " " <> show e <> ")"
    Named s e -> "(Named " <> show s <> " " <> show e <> ")"
    MissingValue -> "MissingValue"
    Custom a -> "(Custom" <> show a <> ")"

-- | Prints a `JsonDecodeError` as a readable error message.
printJsonDecodeError :: JsonDecodeError -> String
printJsonDecodeError = printJsonDecodeError' absurd

printJsonDecodeError' :: forall a. (a -> String) -> JsonDecodeError' a -> String
printJsonDecodeError' printCustomCtor err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
  go = case _ of
    TypeMismatch ty -> "  Expected value of type '" <> ty <> "'."
    UnexpectedValue val -> "  Unexpected value " <> stringify val <> "."
    AtIndex ix inner -> "  At array index " <> show ix <> ":\n" <> go inner
    AtKey key inner -> "  At object key \'" <> key <> "\':\n" <> go inner
    Named name inner -> "  Under '" <> name <> "':\n" <> go inner
    MissingValue -> "  No value was found."
    Custom a -> printCustomCtor a
