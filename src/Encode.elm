module Encode
    exposing
        ( Element
        , Symbol
        , encode
        , float
        , int
        , keyword
        , list
        , mustObject
        , mustSymbol
        , mustTag
        , object
        , string
        , symbol
        , tag
        , toSymbol
        )

{-| Encoding EDN elements


# Basics

@docs Element, encode


# Encoding basic types

@docs string, int, float, symbol, keyword


# Symbols

@docs Symbol, toSymbol, mustSymbol


# Containers

@docs list


# Objects and tags

@docs object, mustObject, tag, mustTag

-}

import Dict
import Json.Encode as Json
import Parse
import Parser exposing ((|.))
import Types


{-| An EDN element.
-}
type alias Element =
    Types.Element ()


{-| An EDN symbol.
-}
type Symbol
    = Symbol String


{-| Create an EDN symbol from an Elm `String`. This will fail
and return `Nothing` if the string is not a valid symbol, compare
the [EDN spec](https://github.com/edn-format/edn#symbols).
-}
toSymbol : String -> Maybe Symbol
toSymbol s =
    case Parser.run (Parse.plainSymbol |. Parser.end) s of
        Ok _ ->
            Just (Symbol s)

        Err err ->
            Nothing


fromSymbol : Symbol -> String
fromSymbol (Symbol s) =
    s


{-| Create an EDN symbol from an Elm `String`, crashing if
it is not a valid symbol.
-}
mustSymbol : String -> Symbol
mustSymbol s =
    case toSymbol s of
        Just ss ->
            ss

        Nothing ->
            Debug.crash <| "not a valid EDN symbol: " ++ s


{-| Make an EDN string from an Elm `String`.
-}
string : String -> Element
string =
    Types.String


{-| Make an EDN symbol from a `Symbol`.
-}
symbol : Symbol -> Element
symbol =
    Types.Symbol << fromSymbol


{-| Make an EDN keyword from a `Symbol`.
-}
keyword : Symbol -> Element
keyword =
    Types.Keyword << fromSymbol


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
pairs of field name symbol and field value.
-}
object : List ( Symbol, Element ) -> Element
object o =
    Types.Map
        (Dict.fromList <| List.map (\( s, e ) -> ( fromSymbol s, e )) <| o)
        []


{-| Make an EDN object (map from keyword to element) from a list of
pairs of field name string and field value. This will crash if any of
the field names are not valid EDN symbols.
-}
mustObject : List ( String, Element ) -> Element
mustObject =
    object << List.map (\( s, e ) -> ( mustSymbol s, e ))


{-| Encode an EDN element to EDN.
-}
encode : Element -> String
encode e =
    case e of
        Types.Int x ->
            toString x

        Types.String s ->
            Json.encode 0 (Json.string s)

        Types.Symbol s ->
            s

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


{-| Make a tagged EDN element from a tag and an element.
-}
tag : Symbol -> Element -> Element
tag s =
    Types.Tagged (fromSymbol s)


{-| Make a tagged EDN element from a `String` and an element.
This will crash if the tag is not a valid EDN symbol.
-}
mustTag : String -> Element -> Element
mustTag s =
    tag (mustSymbol s)
