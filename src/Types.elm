module Types exposing (..)

{-| EDN Types

@docs Element

-}

import Dict exposing (Dict)


{-| An EDN element
-}
type Element a
    = Nil
    | Bool Bool
    | String String
    | Char String
    | Symbol String
    | Keyword String
    | Int Int
    | BigInt { sign : String, digits : String }
    | Float Float
    | BigFloat { sign : String, digits : String } String { sign : String, digits : String }
    | Set (List (Element a))
    | List (List (Element a))
    | Vector (List (Element a))
    | Map (Dict String (Element a)) (List ( Element a, Element a ))
    | Tagged String (Element a)
    | Custom a
