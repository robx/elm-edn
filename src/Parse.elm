module Parse exposing (value)

{-| Parsing EDN


# Basic parsers

@docs value

-}

import Char
import List
import Parser exposing (..)
import String
import Types exposing (..)


seqRest end =
    let
        rest items =
            oneOf
                [ spaceSep
                    |- oneOf
                        [ lazy (\_ -> value) |> andThen (\v -> rest (v :: items))
                        , succeed (List.reverse items) |. symbol end
                        ]
                , succeed (List.reverse items)
                    |. symbol end
                ]
    in
    space
        |- oneOf
            [ lazy (\_ -> value) |> andThen (\v -> rest [ v ])
            , succeed [] |. symbol end
            ]


{-| Parse an EDN value
-}
value : Parser Value
value =
    let
        listRest =
            succeed List |= seqRest ")"
    in
    oneOf
        [ nil
        , integer
        , bool
        , symbol "\"" |- stringRest
        , symbol "(" |- listRest
        , symbol "[" |- vectorRest
        , symbol "{" |- mapRest
        , symbol "#{" |- setRest
        ]


{-| Parse an EDN nil value
-}
nil : Parser Value
nil =
    succeed Nil |. Parser.symbol "nil"


{-| Parse an EDN integer
-}
integer : Parser Value
integer =
    succeed Int
        |= oneOf
            [ succeed ((*) -1)
                |. symbol "-"
                |= int
            , succeed identity
                |. symbol "+"
                |= int
            , int
            ]


{-| Parse an EDN arbitrary precision integer
-}
bigInteger : Parser Value
bigInteger =
    succeed BigInt
        |= int
        |. symbol "N"


isSpace : Char -> Bool
isSpace c =
    c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\x0D'


space : Parser ()
space =
    Parser.ignore Parser.zeroOrMore isSpace


spaceSep : Parser ()
spaceSep =
    Parser.ignore Parser.oneOrMore isSpace


{-| Parse an EDN bool
-}
bool : Parser Value
bool =
    succeed Bool
        |= oneOf
            [ succeed True |. keyword "true"
            , succeed False |. keyword "false"
            ]


{-| Parses an EDN string
-}
stringRest : Parser Value
stringRest =
    let
        esc c =
            case c of
                "t" ->
                    "\t"

                "n" ->
                    "\n"

                "r" ->
                    "\x0D"

                _ ->
                    c

        part =
            oneOf
                [ keep oneOrMore (\c -> c /= '\\' && c /= '"')
                , succeed esc
                    |. symbol "\\"
                    |= keep (Exactly 1) (always True)
                ]
    in
    succeed (String << String.concat)
        |= repeat zeroOrMore part
        |. symbol "\""


{-| unicodeChar translates a four character hexadecimal string
to the character for the corresponding UTF-16 code point
-}
unicodeChar : String -> Char
unicodeChar u =
    Debug.crash "not implemented"


char : Parser Value
char =
    let
        stringToChar s =
            case String.uncons s of
                Just ( c, "" ) ->
                    c

                _ ->
                    Debug.crash "bad single-char string"
    in
    succeed Char
        |. symbol "\\"
        |= oneOf
            [ succeed '\n' |. keyword "newline"
            , succeed '\x0D' |. keyword "return"
            , succeed ' ' |. keyword "space"
            , succeed '\t' |. keyword "tab"
            , succeed unicodeChar
                |. symbol "u"
                |= keep (Exactly 4) Char.isHexDigit
            , succeed stringToChar
                |= keep (Exactly 1) (always True)
            ]


vectorRest =
    succeed Vector |= seqRest "]"


mapRest =
    let
        split xs =
            case xs of
                [] ->
                    Just []

                k :: v :: ys ->
                    Maybe.map ((::) ( k, v )) (split ys)

                _ ->
                    Nothing
    in
    seqRest "}"
        |> andThen
            (\xs ->
                case split xs of
                    Nothing ->
                        fail "expected an even number of map elements"

                    Just ps ->
                        succeed (Map ps)
            )


setRest =
    succeed Set |= seqRest "}"


(|-) p q =
    succeed identity |. p |= q



{-
   | Symbol String
   | Keyword String
   | Float Float
   | BigFloat Float
   | Set (List Value)
   | Map (List (Value, Value))
   | Tagged String Value

-}
