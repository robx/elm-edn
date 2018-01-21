module Parse exposing (genwords, value)

{-| Parsing EDN


# Helpers

@docs genwords


# Basic parsers

@docs value

-}

import Char
import List
import Parser exposing (..)
import String
import Types exposing (..)


{-| sequencing helper
-}
genwords : Parser () -> Parser a -> Parser a -> Parser () -> Parser (List a)
genwords sep boundedWord unboundedWord after =
    let
        finish =
            [] |* after

        anyWord =
            oneOf
                [ boundedWord
                , unboundedWord
                ]

        oneAndThenMore w ws =
            w |> andThen (\x -> map (\xs -> x :: xs) ws)

        boundedAndMore =
            oneAndThenMore boundedWord (lazy (\_ -> words))

        unboundedAndMore =
            oneAndThenMore unboundedWord (lazy (\_ -> moreWords))

        words =
            space
                |- oneOf
                    [ finish
                    , oneOf [ lazy (\_ -> boundedAndMore), lazy (\_ -> unboundedAndMore) ]
                    , delayedCommit sep <|
                        oneOf [ lazy (\_ -> boundedAndMore), lazy (\_ -> unboundedAndMore) ]
                    ]

        moreWords =
            oneOf
                [ finish
                , lazy (\_ -> boundedAndMore)
                , delayedCommit sep <|
                    oneOf [ lazy (\_ -> boundedAndMore), lazy (\_ -> unboundedAndMore) ]
                ]
    in
    words


seq : String -> String -> Parser (List Value)
seq start end =
    symbol start
        |- genwords
            spaceSep
            boundedValue
            unboundedValue
            (delayedCommit space <| symbol end)


boundedValue =
    lazy <| \_ -> oneOf [ list, vector, mapp, set ]


unboundedValue =
    lazy <| \_ -> oneOf [ nil, integer, bool, string, ednSymbol, ednKeyword ]


{-| Parse an EDN value
-}
value : Parser Value
value =
    lazy <|
        \_ ->
            oneOf [ boundedValue, unboundedValue ]


{-| Parse an EDN nil value
-}
nil : Parser Value
nil =
    Nil |* Parser.symbol "nil"


{-| Parse an EDN integer
-}
integer : Parser Value
integer =
    Int
        |$ oneOf
            [ (*) -1
                |* symbol "-"
                |= int
            , symbol "+"
                |- int
            , int
            ]


{-| Parse an EDN arbitrary precision integer
-}
bigInteger : Parser Value
bigInteger =
    BigInt
        |$ int
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
    Bool
        |$ oneOf
            [ True |* keyword "true"
            , False |* keyword "false"
            ]


{-| Parses an EDN string
-}
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
                , esc
                    |* symbol "\\"
                    |= keep (Exactly 1) (always True)
                ]
    in
    (String << String.concat)
        |* symbol "\""
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
    Char
        |* symbol "\\"
        |= oneOf
            [ '\n' |* keyword "newline"
            , '\x0D' |* keyword "return"
            , ' ' |* keyword "space"
            , '\t' |* keyword "tab"
            , unicodeChar
                |* symbol "u"
                |= keep (Exactly 4) Char.isHexDigit
            , stringToChar
                |$ keep (Exactly 1) (always True)
            ]


list =
    List |$ seq "(" ")"


vector =
    Vector |$ seq "[" "]"


mapp =
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
    seq "{" "}"
        |> andThen
            (\xs ->
                case split xs of
                    Nothing ->
                        fail "expected an even number of map elements"

                    Just ps ->
                        succeed (Map ps)
            )


set =
    Set |$ seq "#{" "}"


(|*) f p =
    succeed f |. p


(|$) f q =
    map f q


(|-) p q =
    p |> andThen (\_ -> q)


class s c =
    String.any ((==) c) s


(|||) p q c =
    p c || q c


plainSymbol : Parser String
plainSymbol =
    -- ignoring the / issue for now
    let
        alpha =
            Char.isUpper ||| Char.isLower

        num =
            Char.isDigit

        alphanum =
            alpha ||| num

        nosecondnum =
            class "-+."

        notfirst =
            class ":#"

        other =
            class "*!_?$%&=<>"
    in
    oneOf
        [ (++)
            |$ keep (Exactly 1) (alpha ||| other)
            |= keep zeroOrMore (alphanum ||| notfirst ||| other)
        , (++)
            |$ keep (Exactly 1) nosecondnum
            |= oneOf
                [ (++)
                    |$ keep (Exactly 1) (alpha ||| notfirst ||| other)
                    |= keep zeroOrMore (alphanum ||| notfirst ||| other)
                , succeed ""
                ]
        ]


ednSymbol : Parser Value
ednSymbol =
    Symbol |$ plainSymbol


ednKeyword : Parser Value
ednKeyword =
    Keyword
        |* symbol ":"
        |= plainSymbol



{-
   | Float Float
   | BigFloat Float
   | Tagged String Value
-}
