module Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , decodeElement
        , decodeString
        , dict
        , element
        , fail
        , field
        , int
        , keyword
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , string
        , succeed
        , tagged
        )

{-| Element decoders


# Primitives

@docs Decoder
@docs string, keyword, bool, int


# Data Structures

@docs field, list, dict, tagged


# Run Decoders

@docs decodeString, decodeElement


# Mapping

@docs map, map2, map3, map4, map5, map6


# Fancy Decoding

@docs andThen, succeed, fail, element

-}

import Dict
import Parse
import Parser
import Types exposing (..)


{-| A value that knows how to decode EDN elements.
-}
type alias Decoder a =
    Element -> Result String a


{-| Parse the given string into an EDN element and then run the Decoder on it.
-}
decodeString : Decoder a -> String -> Result String a
decodeString d s =
    Parser.run Parse.element s
        |> Result.mapError toString
        |> Result.andThen d


{-| Run a Decoder on some JSON Element.
-}
decodeElement : Decoder a -> Element -> Result String a
decodeElement =
    identity


{-| Transform a decoder.
-}
map : (a -> value) -> Decoder a -> Decoder value
map f d =
    Result.map f << d


{-| -}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 f d1 d2 e =
    Result.map2 f (d1 e) (d2 e)


{-| -}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 f d1 d2 d3 e =
    Result.map3 f (d1 e) (d2 e) (d3 e)


{-| -}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 f d1 d2 d3 d4 e =
    Result.map4 f (d1 e) (d2 e) (d3 e) (d4 e)


{-| -}
map5 : (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 f d1 d2 d3 d4 d5 =
    map4 f d1 d2 d3 d4 |> andThen (\g -> map g d5)


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
map6 f d1 d2 d3 d4 d5 d6 =
    map5 f d1 d2 d3 d4 d5
        |> andThen (\g -> map g d6)


{-| -}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f p e =
    p e |> Result.andThen (\x -> f x e)


{-| -}
succeed : a -> Decoder a
succeed x =
    always (Ok x)


{-| -}
fail : String -> Decoder a
fail err =
    always (Err err)


{-| Do not do anything with an EDN element, just bring it into Elm as a Element.
-}
element : Decoder Element
element =
    Ok


wrongType : Element -> Element -> Result String a
wrongType want have =
    let
        desc el =
            case el of
                String _ ->
                    "a string"

                Int _ ->
                    "an integer"

                Nil ->
                    "`nil`"

                Symbol _ ->
                    "a symbol"

                Keyword _ ->
                    "a keyword"

                Tagged _ _ ->
                    "a tagged element"

                List _ ->
                    "a list"

                Set _ ->
                    "a set"

                Map _ _ ->
                    "a map"

                Float _ ->
                    "a floating point number"

                BigInt _ ->
                    "a big integer"

                BigFloat _ ->
                    "a big floating point number"

                Bool _ ->
                    "a boolean"

                Char _ ->
                    "a character"

                Vector _ ->
                    "a vector"
    in
    Err <| "expected " ++ desc want ++ " but found " ++ desc have


{-| Decode an EDN string into an Elm String.
-}
string : Decoder String
string e =
    case e of
        String s ->
            Ok s

        _ ->
            wrongType (String "") e


{-| Decode an EDN integer into an Elm Int.
-}
int : Decoder Int
int e =
    case e of
        Int x ->
            Ok x

        _ ->
            wrongType (Int 0) e


{-| Decode an EDN keyword into an Elm String.
-}
keyword : Decoder String
keyword e =
    case e of
        Keyword s ->
            Ok s

        _ ->
            wrongType (Keyword "") e


{-| Decode an EDN boolean into an Elm Bool.
-}
bool : Decoder Bool
bool e =
    case e of
        Bool b ->
            Ok b

        _ ->
            wrongType (Bool False) e


{-| Decode an EDN list into an Elm List.
-}
list : Decoder a -> Decoder (List a)
list d e =
    let
        listHelp l =
            case l of
                [] ->
                    Ok []

                f :: fs ->
                    (::) <$> decodeElement d f <*> listHelp fs
    in
    case e of
        List l ->
            listHelp l

        _ ->
            wrongType (List []) e


assocList : Decoder key -> Decoder value -> Decoder (List ( key, value ))
assocList key value e =
    let
        merge keyed unkeyed =
            (Dict.toList keyed |> List.map (\( k, v ) -> ( Keyword k, v ))) ++ unkeyed
    in
    case e of
        Map keyed unkeyed ->
            let
                rec xs =
                    case xs of
                        ( k, v ) :: ys ->
                            Result.map2 (,) (key k) (value v)
                                |> Result.andThen (\x -> Result.map ((::) x) (rec ys))

                        [] ->
                            Ok []
            in
            rec (merge keyed unkeyed)

        _ ->
            wrongType (Map Dict.empty []) e


{-| Decode an EDN map into an Elm Dict.
-}
dict : Decoder comparable -> Decoder value -> Decoder (Dict.Dict comparable value)
dict key value =
    map Dict.fromList (assocList key value)


{-| Decode an object encoded as a map from keywords to element
-}
object : Decoder (Dict.Dict String Element)
object e =
    case e of
        Map keyed [] ->
            Ok keyed

        Map _ unkeyed ->
            Err <| "non-keyword keys in map: " ++ toString unkeyed

        _ ->
            wrongType (Map Dict.empty []) e


{-| Decode an EDN map field.
-}
field : String -> Decoder a -> Decoder a
field f d =
    map (Dict.get f) object
        |> andThen
            (\maybeElement _ ->
                case maybeElement of
                    Just e ->
                        d e

                    Nothing ->
                        Err ("field not found: " ++ f)
            )


{-| Decode an element based on its tag.
-}
tagged : List ( String, Decoder a ) -> Decoder a
tagged decoders e =
    case e of
        Tagged t f ->
            case Dict.get t (Dict.fromList decoders) of
                Just d ->
                    d f

                Nothing ->
                    Err <| "unknown tag: " ++ t

        _ ->
            wrongType (Tagged "" Nil) e


(<$>) : (a -> b) -> Result err a -> Result err b
(<$>) =
    Result.map


(<*>) : Result err (a -> b) -> Result err a -> Result err b
(<*>) f p =
    f |> Result.andThen (\g -> g <$> p)
