## Module Data.Argonaut.Decode

#### `DecodeJson`

``` purescript
class DecodeJson a where
  decodeJson :: Json -> Either String a
```

##### Instances
``` purescript
instance decodeJsonMaybe :: (DecodeJson a) => DecodeJson (Maybe a)
instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b)
instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b)
instance decodeJsonNull :: DecodeJson Unit
instance decodeJsonBoolean :: DecodeJson Boolean
instance decodeJsonNumber :: DecodeJson Number
instance decodeJsonInt :: DecodeJson Int
instance decodeJsonString :: DecodeJson String
instance decodeJsonJson :: DecodeJson Json
instance decodeJsonChar :: DecodeJson Char
instance decodeStrMap :: (DecodeJson a) => DecodeJson (StrMap a)
instance decodeArray :: (DecodeJson a) => DecodeJson (Array a)
instance decodeList :: (DecodeJson a) => DecodeJson (List a)
instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map a b)
```

#### `decodeMaybe`

``` purescript
decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
```


