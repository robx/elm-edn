module Parse exposing (element)

{-| Parsing EDN


# Basic parsers

@docs element

-}

import Char
import Dict
import List
import Parser exposing (..)
import String
import Types exposing (..)


seq : String -> String -> Parser (List Element)
seq open close =
    identity
        |* symbol open
        |. space
        |= elements
        |. symbol close


discard : Parser ()
discard =
    inContext "discard" <|
        symbol "#_"
            |. space
            |. (lazy <| \_ -> element)


discardOrElement : Parser (Maybe Element)
discardOrElement =
    lazy <|
        \_ ->
            oneOf
                [ Nothing |* discard
                , Just |$ element
                ]


elements : Parser (List Element)
elements =
    List.filterMap identity
        |$ repeat zeroOrMore (lazy (\_ -> discardOrElement) |. space)


{-| Parse an EDN element
-}
element : Parser Element
element =
    lazy <| \_ -> oneOf [ identity |* discard |. space |= element, realElement ]


realElement : Parser Element
realElement =
    lazy <|
        \_ ->
            oneOf
                [ list
                , vector
                , mapp
                , set
                , number
                , string
                , symbols
                , ednKeyword
                , tagged
                , char
                ]


sep : Parser ()
sep =
    oneOf
        [ spaceSep
        , Parser.lookAhead <|
            oneOf
                [ symbol "("
                , symbol "["
                , symbol "{"
                , symbol ")"
                , symbol "]"
                , symbol "}"
                , symbol "\""
                , end
                ]
        ]


{-| Parse an EDN integer
-}
integer : Parser Element
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
        |. sep


isSpace : Char -> Bool
isSpace c =
    c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\x0D'


space : Parser ()
space =
    Parser.ignore Parser.zeroOrMore isSpace


spaceSep : Parser ()
spaceSep =
    Parser.ignore Parser.oneOrMore isSpace


{-| Parses an EDN string
-}
string : Parser Element
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


char : Parser Element
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
        |. sep


list =
    inContext "list" <|
        List
            |$ (lazy <| \_ -> seq "(" ")")


vector =
    inContext "vector" <|
        Vector
            |$ (lazy <| \_ -> seq "[" "]")


mapp =
    let
        build keyed unkeyed elements =
            case elements of
                key :: value :: rest ->
                    case key of
                        Keyword keyword ->
                            build (Dict.insert keyword value keyed) unkeyed rest

                        _ ->
                            build keyed (( key, value ) :: unkeyed) rest

                [] ->
                    succeed <| Map keyed (List.reverse unkeyed)

                _ ->
                    fail "uneven number of map elements"
    in
    inContext "map" <|
        ((lazy <| \_ -> seq "{" "}")
            |> andThen (build Dict.empty [])
        )


set =
    inContext "set" <|
        Set
            |$ (lazy <| \_ -> seq "#{" "}")


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
            class "*!_?$%&=<>/"
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
        |. sep


{-| symbols parses EDN symbols and true/false/nil
-}
symbols : Parser Element
symbols =
    let
        f s =
            case s of
                "true" ->
                    Bool True

                "false" ->
                    Bool False

                "nil" ->
                    Nil

                _ ->
                    Symbol s
    in
    f |$ plainSymbol


ednKeyword : Parser Element
ednKeyword =
    Keyword
        |* symbol ":"
        |= plainSymbol


tagged : Parser Element
tagged =
    inContext "tagged" <|
        Tagged
            |* symbol "#"
            |= plainSymbol
            |= element


type alias RawInteger =
    { sign : String
    , digits : String
    }


type alias RawNumber =
    { integer : RawInteger
    , frac : Maybe String
    , exp : Maybe RawInteger
    , big : Maybe String
    }


rawNumber : Parser RawNumber
rawNumber =
    let
        sign =
            oneOf
                [ keep (Exactly 1) (\c -> c == '+' || c == '-')
                , succeed "+"
                ]

        digits =
            oneOf
                [ keep (Exactly 1) (\c -> c == '0')
                , (++)
                    |$ keep (Exactly 1) (\c -> Char.isDigit c && c /= '0')
                    |= keep zeroOrMore (\c -> Char.isDigit c)
                ]

        integer =
            RawInteger
                |$ sign
                |= digits

        frac =
            symbol "." |- keep zeroOrMore Char.isDigit

        exp =
            RawInteger
                |* oneOf [ symbol "e", symbol "E" ]
                |= sign
                |= digits

        big =
            keep (Exactly 1) (\c -> c == 'M' || c == 'N')
    in
    RawNumber
        |$ integer
        |= oneOf [ map Just frac, succeed Nothing ]
        |= oneOf [ map Just exp, succeed Nothing ]
        |= oneOf [ map Just big, succeed Nothing ]
        |. sep


{-| -}
number : Parser Element
number =
    let
        rawToStr raw =
            raw.sign ++ raw.digits

        rawToInt raw =
            case String.toInt (rawToStr raw) of
                Ok v ->
                    v

                Err err ->
                    Debug.crash <| "failed to parse integer: " ++ rawToStr raw ++ ": " ++ err

        strToFloat s =
            case String.toFloat s of
                Ok v ->
                    v

                Err err ->
                    Debug.crash <| "failed to parse float: " ++ s ++ ": " ++ err

        f n =
            case ( n.frac, n.exp, n.big ) of
                ( Nothing, Nothing, Nothing ) ->
                    succeed <| Int <| rawToInt n.integer

                ( Nothing, Nothing, Just "N" ) ->
                    succeed <| BigInt n.integer

                ( _, _, Nothing ) ->
                    succeed <|
                        Float <|
                            strToFloat <|
                                String.concat
                                    [ n.integer.sign
                                    , n.integer.digits
                                    , "."
                                    , Maybe.withDefault "0" n.frac
                                    , "E"
                                    , rawToStr (Maybe.withDefault { sign = "+", digits = "0" } n.exp)
                                    ]

                ( _, _, Just "M" ) ->
                    succeed <|
                        BigFloat n.integer
                            (Maybe.withDefault "0" n.frac)
                            (Maybe.withDefault { sign = "+", digits = "0" } n.exp)

                _ ->
                    fail "mix of integer and floating point"
    in
    rawNumber
        |> andThen f
