module Decode
    exposing
        ( Decoder
        , bool
        , decodeElement
        , decodeString
        , element
        , keyword
        , list
        , string
        )

{-| Element decoders


# Primitives

@docs Decoder
@docs string
@docs keyword
@docs bool


# Data Structures

@docs list


# Run Decoders

@docs decodeString
@docs decodeElement


# Fancy Decoding

@docs element

-}

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


{-| Do not do anything with an EDN element, just bring it into Elm as a Element.
-}
element : Decoder Element
element =
    Ok


{-| Decode an EDN string into an Elm string.
-}
string : Decoder String
string e =
    case e of
        String s ->
            Ok s

        _ ->
            Err "not a string"


{-| Decode an EDN keyword into an Elm string.
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
