## Module Data.Argonaut.Combinators

#### `(:=)`

``` purescript
(:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
```

_non-associative / precedence 7_

#### `(~>)`

``` purescript
(~>) :: forall a. (EncodeJson a) => JAssoc -> a -> Json
```

_right-associative / precedence 6_

#### `(?>>=)`

``` purescript
(?>>=) :: forall a b. Maybe a -> String -> Either String a
```

_left-associative / precedence 1_

#### `(.?)`

``` purescript
(.?) :: forall a. (DecodeJson a) => JObject -> String -> Either String a
```

_non-associative / precedence 7_


