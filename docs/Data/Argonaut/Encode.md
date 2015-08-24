## Module Data.Argonaut.Encode

#### `EncodeJson`

``` purescript
class EncodeJson a where
  encodeJson :: a -> Json
```

##### Instances
``` purescript
instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a)
instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b)
instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b)
instance encodeJsonUnit :: EncodeJson Unit
instance encodeJsonJBoolean :: EncodeJson Boolean
instance encodeJsonJNumber :: EncodeJson Number
instance encodeJsonInt :: EncodeJson Int
instance encodeJsonJString :: EncodeJson String
instance encodeJsonJson :: EncodeJson Json
instance encodeJsonChar :: EncodeJson Char
instance encodeJsonArray :: (EncodeJson a) => EncodeJson (Array a)
instance encodeJsonList :: (EncodeJson a) => EncodeJson (List a)
instance encodeStrMap :: (EncodeJson a) => EncodeJson (StrMap a)
instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (Map a b)
```

#### `gEncodeJson`

``` purescript
gEncodeJson :: forall a. (Generic a) => a -> Json
```

Encode any `Generic` data structure into `Json`.

#### `gEncodeJson'`

``` purescript
gEncodeJson' :: GenericSpine -> Json
```

Encode `GenericSpine` into `Json`.


