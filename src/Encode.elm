module Encode
    exposing
        ( Element
        , encode
        , float
        , int
        , list
        , object
        , string
        , tag
        )

{-| Encoding EDN elements

@docs Element, encode, string, int, float, object, list, tag

-}

import Dict
import Json.Encode as Json
import Types


{-| An EDN element.
-}
type alias Element =
    Types.Element


{-| Make an EDN string from an Elm `String`.
-}
string : String -> Element
string =
    Types.String


{-| Make an EDN integer from an Elm `Int`.
-}
int : Int -> Element
int =
    Types.Int


{-| Make an EDN floating point number from an Elm `Float`.
-}
float : Float -> Element
float =
    Types.Float


{-| Make an EDN list from an Elm `List` of EDN elements.
-}
list : List Element -> Element
list =
    Types.List


{-| Make an EDN object (map from keyword to element) from a list of
pairs of field name and field value.
-}
object : List ( String, Element ) -> Element
object o =
    Types.Map
        (Dict.fromList o)
        []


{-| Encode an EDN element to EDN.
-}
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


{-| Make a tagged EDN element from a tag and and element.
-}
tag : String -> Element -> Element
tag =
    Types.Tagged
