module Decode
    exposing
        ( Decoder
        , bool
        , decodeElement
        , decodeString
        , element
        , field
        , int
        , keyword
        , list
        , map
        , string
        )

{-| Element decoders


# Primitives

@docs Decoder
@docs string, keyword, bool, int


# Data Structures

@docs field, list


# Run Decoders

@docs decodeString, decodeElement


# Mapping

@docs map


# Fancy Decoding

@docs element

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


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f p e =
    p e |> Result.andThen (\x -> f x e)


{-| Do not do anything with an EDN element, just bring it into Elm as a Element.
-}
element : Decoder Element
element =
    Ok


{-| Decode an EDN string into an Elm String.
-}
string : Decoder String
string e =
    case e of
        String s ->
            Ok s

        _ ->
            Err "not a string"


{-| Decode an EDN integer into an Elm Int.
-}
int : Decoder Int
int e =
    case e of
        Int x ->
            Ok x

        _ ->
            Err "not an integer"


{-| Decode an EDN keyword into an Elm String.
-}
keyword : Decoder String
keyword e =
    case e of
        Keyword s ->
            Ok s

        _ ->
            Err "not a keyword"


{-| Decode an EDN boolean into an Elm Bool.
-}
bool : Decoder Bool
bool e =
    case e of
        Bool b ->
            Ok b

        _ ->
            Err "not a bool"


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
            Err "not a list"


assocList : Decoder key -> Decoder value -> Decoder (List ( key, value ))
assocList key value e =
    case e of
        Map ps ->
            let
                rec xs =
                    case xs of
                        ( k, v ) :: ys ->
                            Result.map2 (,) (key k) (value v)
                                |> Result.andThen (\x -> Result.map ((::) x) (rec ys))

                        [] ->
                            Ok []
            in
            rec ps

        _ ->
            Err "not a map"


{-| Decode an EDN map into an Elm Dict.
-}
dict : Decoder comparable -> Decoder value -> Decoder (Dict.Dict comparable value)
dict key value =
    map Dict.fromList (assocList key value)


{-| Decode an object encoded as a map from keywords to element
-}
object : Decoder (Dict.Dict String Element)
object =
    dict keyword element


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
                        Err "field not found"
            )


tagged : (String -> Decoder a) -> Decoder a
tagged lookup e =
    case e of
        Tagged t f ->
            lookup t f

        _ ->
            Err "not a tagged element"


(<$>) : (a -> b) -> Result err a -> Result err b
(<$>) =
    Result.map


(<*>) : Result err (a -> b) -> Result err a -> Result err b
(<*>) f p =
    f |> Result.andThen (\g -> g <$> p)
