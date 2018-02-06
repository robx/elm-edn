module Parse exposing (onlyElement, onlyElements, plainSymbol)

{-| Parsing EDN


# Basic parsers

@docs onlyElement, onlyElements, plainSymbol

-}

import Char
import Dict
import List
import Parser as P exposing ((|.), (|=), Parser)
import String
import Types exposing (..)


type alias Config a =
    String -> Maybe (Element a -> Result String a)


type alias CustomParser a b =
    Config a -> Parser b


seq : String -> String -> CustomParser a (List (Element a))
seq open close config =
    P.succeed identity
        |. P.symbol open
        |= elements config
        |. P.symbol close


discard : Parser ()
discard =
    P.inContext "discard" <|
        P.symbol "#_"
            |. P.lazy (\_ -> element (always Nothing))


comment : Parser ()
comment =
    P.symbol ";"
        |. P.ignore P.zeroOrMore (\c -> c /= '\n')
        |. P.oneOf [ P.symbol "\n", P.end ]


junk : Parser ()
junk =
    P.lazy (\_ -> P.oneOf [ discard, comment, spaceSep ])


{-| Parse any number of EDN elements.

Any whitespace, comments and discarded elements at the start
of the input will be consumed. What comes behind doesn't matter.

-}
elements : CustomParser a (List (Element a))
elements config =
    let
        junkOrElement =
            P.lazy <|
                \_ ->
                    P.oneOf
                        [ P.succeed Nothing |. junk
                        , P.succeed Just |= element config
                        ]
    in
    P.succeed (List.filterMap identity)
        |= P.repeat P.zeroOrMore junkOrElement


{-| Parse a single EDN element.

Any whitespace, comments and discarded elements before the element
will be consumed. What comes behind doesn't matter.

-}
element : CustomParser a (Element a)
element config =
    P.lazy <|
        \_ ->
            P.succeed identity
                |. P.repeat P.zeroOrMore junk
                |= realElement config


{-| Parse a single EDN element.

Whitepace, comments and discarded elements before and after the
element will be consumed. If there is anything else the parser
will fail.

-}
onlyElement : CustomParser a (Element a)
onlyElement config =
    element config |. P.repeat P.zeroOrMore junk |. P.end


{-| Parse any number of EDN elements.

The whole input will be consumed

-}
onlyElements : CustomParser a (List (Element a))
onlyElements config =
    elements config |. P.end


realElement : CustomParser a (Element a)
realElement config =
    P.lazy <|
        \_ ->
            P.oneOf
                [ list config
                , vector config
                , map config
                , set config
                , number
                , string
                , symbols
                , ednKeyword
                , tagged config
                , char
                ]


sep : Parser ()
sep =
    P.oneOf
        [ spaceSep
        , P.lookAhead <|
            P.oneOf
                [ P.symbol "("
                , P.symbol "["
                , P.symbol "{"
                , P.symbol ")"
                , P.symbol "]"
                , P.symbol "}"
                , P.symbol "\""
                , P.end
                ]
        ]


isSpace : Char -> Bool
isSpace c =
    c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\x0D'


space : Parser ()
space =
    P.ignore P.zeroOrMore isSpace


spaceSep : Parser ()
spaceSep =
    P.ignore P.oneOrMore isSpace


{-| Parses an EDN string
-}
string : Parser (Element a)
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
            P.oneOf
                [ P.keep P.oneOrMore (\c -> c /= '\\' && c /= '"')
                , P.succeed esc
                    |. P.symbol "\\"
                    |= P.keep (P.Exactly 1) (always True)
                ]
    in
    P.succeed (String << String.concat)
        |. P.symbol "\""
        |= P.repeat P.zeroOrMore part
        |. P.symbol "\""


char : Parser (Element a)
char =
    P.succeed Char
        |. P.symbol "\\"
        |= P.source
            (P.oneOf
                [ P.keyword "newline"
                , P.keyword "return"
                , P.keyword "space"
                , P.keyword "tab"
                , P.symbol "u" |. P.ignore (P.Exactly 4) Char.isHexDigit
                , P.ignore (P.Exactly 1) (always True)
                ]
            )
        |. sep


list : CustomParser a (Element a)
list config =
    P.inContext "list" <|
        P.succeed List
            |= (P.lazy <| \_ -> seq "(" ")" config)


vector : CustomParser a (Element a)
vector config =
    P.inContext "vector" <|
        P.succeed Vector
            |= (P.lazy <| \_ -> seq "[" "]" config)


map : CustomParser a (Element a)
map config =
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
                    P.succeed <| Map keyed (List.reverse unkeyed)

                _ ->
                    P.fail "uneven number of map elements"
    in
    P.inContext "map" <|
        ((P.lazy <| \_ -> seq "{" "}" config)
            |> P.andThen (build Dict.empty [])
        )


set : CustomParser a (Element a)
set config =
    P.inContext "set" <|
        P.succeed Set
            |= (P.lazy <| \_ -> seq "#{" "}" config)


class : String -> Char -> Bool
class s c =
    String.any ((==) c) s


(|||) : (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q c =
    p c || q c


{-| plainSymbol parses the symbol-part of EDN symbols, keywords, tags.
-}
plainSymbol : Parser String
plainSymbol =
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

        part =
            P.oneOf
                [ P.succeed (++)
                    |= P.keep (P.Exactly 1) (alpha ||| other)
                    |= P.keep P.zeroOrMore (alphanum ||| nosecondnum ||| notfirst ||| other)
                , P.succeed (++)
                    |= P.keep (P.Exactly 1) nosecondnum
                    |= P.oneOf
                        [ P.succeed (++)
                            |= P.keep (P.Exactly 1) (alpha ||| notfirst ||| other)
                            |= P.keep P.zeroOrMore (alphanum ||| nosecondnum ||| notfirst ||| other)
                        , P.succeed ""
                        ]
                ]
    in
    P.oneOf
        [ P.succeed "/"
            |. P.symbol "/"
        , P.succeed (++)
            |= part
            |= P.oneOf
                [ P.succeed ((++) "/")
                    |. P.symbol "/"
                    |= part
                , P.succeed ""
                ]
        ]


{-| symbols parses EDN symbols and true/false/nil
-}
symbols : Parser (Element a)
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
    P.succeed f
        |= plainSymbol
        |. sep


ednKeyword : Parser (Element a)
ednKeyword =
    P.succeed Keyword
        |. P.symbol ":"
        |= plainSymbol
        |. sep


tagged : CustomParser a (Element a)
tagged config =
    let
        tag : CustomParser a ( String, Element a )
        tag cfg =
            P.succeed (,)
                |. P.symbol "#"
                |= plainSymbol
                |. sep
                |= element cfg

        f : Config a -> ( String, Element a ) -> Parser (Element a)
        f cfg ( t, e ) =
            case cfg t of
                Just dec ->
                    case dec e of
                        Ok v ->
                            P.succeed (Custom v)

                        Err err ->
                            P.fail <| "custom parser for tag " ++ t ++ ": " ++ err

                Nothing ->
                    P.succeed <| Tagged t e
    in
    P.inContext "tagged" <|
        (tag config
            |> P.andThen (f config)
        )


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
            P.oneOf
                [ P.keep (P.Exactly 1) (\c -> c == '+' || c == '-')
                , P.succeed "+"
                ]

        digits =
            P.oneOf
                [ P.keep (P.Exactly 1) (\c -> c == '0')
                , P.succeed (++)
                    |= P.keep (P.Exactly 1) (\c -> Char.isDigit c && c /= '0')
                    |= P.keep P.zeroOrMore (\c -> Char.isDigit c)
                ]

        integer =
            P.succeed RawInteger
                |= sign
                |= digits

        frac =
            P.succeed identity
                |. P.symbol "."
                |= P.keep P.zeroOrMore Char.isDigit

        exp =
            P.succeed RawInteger
                |. P.oneOf [ P.symbol "e", P.symbol "E" ]
                |= sign
                |= digits

        big =
            P.keep (P.Exactly 1) (\c -> c == 'M' || c == 'N')
    in
    P.succeed RawNumber
        |= integer
        |= P.oneOf [ P.map Just frac, P.succeed Nothing ]
        |= P.oneOf [ P.map Just exp, P.succeed Nothing ]
        |= P.oneOf [ P.map Just big, P.succeed Nothing ]
        |. sep


number : Parser (Element a)
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
                    P.succeed <| Int <| rawToInt n.integer

                ( Nothing, Nothing, Just "N" ) ->
                    P.succeed <| BigInt n.integer

                ( _, _, Nothing ) ->
                    P.succeed <|
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
                    P.succeed <|
                        BigFloat n.integer
                            (Maybe.withDefault "0" n.frac)
                            (Maybe.withDefault { sign = "+", digits = "0" } n.exp)

                _ ->
                    P.fail "mix of integer and floating point"
    in
    rawNumber
        |> P.andThen f
