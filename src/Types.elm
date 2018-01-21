module Types exposing (..)

{-| EDN Types

@docs Element

-}


{-| An EDN element
-}
type Element
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
    | Set (List Element)
    | List (List Element)
    | Vector (List Element)
    | Map (List ( Element, Element ))
    | Tagged String Element
