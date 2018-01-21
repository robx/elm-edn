module Decode
    exposing
        ( Decoder
        , bool
        , decodeString
        , decodeValue
        , keyword
        , list
        , string
        , value
        )

{-| Value decoders


# Primitives

@docs Decoder
@docs string
@docs keyword
@docs bool


# Data Structures

@docs list


# Run Decoders

@docs decodeString
@docs decodeValue


# Fancy Decoding

@docs value

-}

import Parse
import Parser
import Types exposing (..)

{-| A value that knows how to decode EDN values. -}
type alias Decoder a =
    Value -> Result String a


{-| Parse the given string into a JSON value and then run the Decoder on it.
-}
decodeString : Decoder a -> String -> Result String a
decodeString d s =
    Parser.run Parse.value s
        |> Result.mapError toString
        |> Result.andThen d


{-| Run a Decoder on some JSON Value.
-}
decodeValue : Decoder a -> Value -> Result String a
decodeValue =
    identity


{-| Do not do anything with an EDN value, just bring it into Elm as a Value.
-}
value : Decoder Value
value = Ok


{-| Decode an EDN string into an Elm string.
-}
string : Decoder String
string v =
    case v of
        String s ->
            Ok s

        _ ->
            Err "not a string"


{-| Decode an EDN keyword into an Elm string.
-}
keyword : Decoder String
keyword v =
    case v of
        Keyword s ->
            Ok s

        _ ->
            Err "not a keyword"


{-| Decode an EDN boolean into an Elm Bool.
-}
bool : Decoder Bool
bool v =
    case v of
        Bool b ->
            Ok b

        _ ->
            Err "not a bool"


{-| Decode an EDN list into an Elm List.
-}
list : Decoder a -> Decoder (List a)
list d v =
    let
        listHelp l =
            case l of
                [] ->
                    Ok []

                w :: ws ->
                    (::) <$> decodeValue d w <*> listHelp ws
    in
    case v of
        List l ->
            listHelp l

        _ ->
            Err "not a list"


tagged : (String -> Decoder a) -> Decoder a
tagged lookup v =
    case v of
        Tagged t w -> lookup t w
        _ -> Err "not a tagged value"


(<$>) : (a -> b) -> Result err a -> Result err b
(<$>) =
    Result.map


(<*>) : Result err (a -> b) -> Result err a -> Result err b
(<*>) f p =
    f |> Result.andThen (\g -> g <$> p)
