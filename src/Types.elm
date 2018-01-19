module Types exposing (..)

{-| EDN Types

@docs Value

-}


{-| Arbitrary raw EDN values
-}
type Value
    = Nil
    | Bool Bool
    | String String
    | Char Char
    | Symbol String
    | Keyword String
    | Int Int
    | BigInt Int
    | Float Float
    | BigFloat Float
    | Set (List Value)
    | List (List Value)
    | Vector (List Value)
    | Map (List ( Value, Value ))
    | Tagged String Value
