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


seq2 : String -> String -> Parser (List Value)
seq2 start end =
    let
        sepValue : Parser Value
        sepValue =
            oneOf
                [ succeed List |= seq2 "(" ")"
                , vector
                , mapp
                , set
                , delayedCommit spaceSep value
                ]

        values : Parser (List Value)
        values =
            oneOf
                [ succeed (::)
                    |= sepValue
                    |= (lazy <| \_ -> values)
                , succeed []
                ]
    in
    symbol start
        |- values
        |. delayedCommit spaceSep (symbol end)


seq : String -> String -> Parser (List Value)
seq start end =
    let
        rest items =
            oneOf
                [ spaceSep
                    |- oneOf
                        [ value |> andThen (\v -> rest (v :: items))
                        , succeed (List.reverse items) |. symbol end
                        ]
                , succeed (List.reverse items)
                    |. symbol end
                ]
    in
    symbol start
        |- space
        |- oneOf
            [ lazy (\_ -> value) |> andThen (\v -> rest [ v ])
            , succeed [] |. symbol end
            ]


{-| Parse an EDN value
-}
value : Parser Value
value =
    lazy <|
        \_ ->
            oneOf
                [ nil
                , integer
                , bool
                , string
                , succeed List |= seq "(" ")"
                , vector
                , mapp
                , set
                , ednSymbol
                , ednKeyword
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


vector =
    succeed Vector |= seq "[" "]"


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
    succeed Set |= seq "#{" "}"


(|-) p q =
    p |> andThen (\_ -> q)



--    succeed identity |. p |= q


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
        [ succeed (++)
            |= keep (Exactly 1) (alpha ||| other)
            |= keep zeroOrMore (alphanum ||| notfirst ||| other)
        , succeed (++)
            |= keep (Exactly 1) nosecondnum
            |= oneOf
                [ succeed (++)
                    |= keep (Exactly 1) (alpha ||| notfirst ||| other)
                    |= keep zeroOrMore (alphanum ||| notfirst ||| other)
                , succeed ""
                ]
        ]


ednSymbol : Parser Value
ednSymbol =
    succeed Symbol |= plainSymbol


ednKeyword : Parser Value
ednKeyword =
    succeed Keyword
        |. symbol ":"
        |= plainSymbol



{-
   | Keyword String
   | Float Float
   | BigFloat Float
   | Tagged String Value
-}
