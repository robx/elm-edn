module Parse
    exposing
        ( bigInteger
        , integer
        )

{-| Parsing EDN


# Basic parsers

@docs integer, bigInteger

-}

import Char
import List
import Parser exposing (..)
import String
import Types exposing (..)


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


bool : Parser Value
bool =
    succeed Bool
        |= oneOf
            [ succeed True |. keyword "true"
            , succeed False |. keyword "false"
            ]


string : Parser Value
string =
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
        |. symbol "\""
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


list : Parser Value
list =
    succeed List
        |. symbol "("
        |. space
        |= oneOf
            [ succeed (::)
                |= lazy (\_ -> value)
                |= repeat zeroOrMore
                    (succeed identity |. spaceSep |= lazy (\_ -> value))
                |. symbol ")"
            , succeed [] |. symbol ")"
            ]


value : Parser Value
value =
    oneOf
        [ nil
        , integer
        , bool
        , string
        , list
        ]



{-
   | Symbol String
   | Keyword String
   | Float Float
   | BigFloat Float
   | Set (List Value)
   | Vector (List Value)
   | Map (List (Value, Value))
   | Tagged String Value

-}
