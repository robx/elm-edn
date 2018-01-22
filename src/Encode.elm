module Encode
    exposing
        ( Element
        , encode
        , int
        , list
        , object
        , string
        , tag
        )

{-| Encoding EDN elements

@docs Element, encode, string, int, object, list, tag

-}

import Dict
import Json.Encode as Json
import Types


{-| -}
type alias Element =
    Types.Element


{-| -}
string : String -> Element
string =
    Types.String


{-| -}
int : Int -> Element
int =
    Types.Int


{-| -}
list : List Element -> Element
list =
    Types.List


{-| -}
object : List ( String, Element ) -> Element
object o =
    Types.Map
        (Dict.fromList o)
        []


{-| -}
encode : Element -> String
encode e =
    case e of
        Types.Int x ->
            toString x

        Types.String s ->
            Json.encode 0 (Json.string s)

        Types.Keyword k ->
            ":" ++ k

        Types.List es ->
            "(" ++ String.join " " (List.map encode es) ++ ")"

        Types.Map keyed unkeyed ->
            let
                merged =
                    (Dict.toList keyed |> List.map (\( k, v ) -> ( Types.Keyword k, v ))) ++ unkeyed

                pair ( k, v ) =
                    encode k ++ " " ++ encode v
            in
            "{" ++ String.join ", " (List.map pair merged) ++ "}"

        Types.Tagged tag element ->
            "#" ++ tag ++ " " ++ encode element

        _ ->
            Debug.crash <| "unsupported element: " ++ toString e


{-| -}
tag : String -> Element -> Element
tag =
    Types.Tagged
