module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser exposing ((|.))
import Test exposing (..)
import Types exposing (..)


type T
    = W
    | Ts (List T)


tword : Parser.Parser T
tword =
    Parser.succeed W |. Parser.symbol "word"


tlist : Parser.Parser T
tlist =
    Parser.symbol "("
        |> Parser.andThen
            (\_ -> Parser.map Ts (Parse.genwords (Parser.symbol " ") tlist tword (Parser.symbol ")")))



--parse exp p s =
--    Expect.equal exp (Parser.run p s)


parse : List ( String, a ) -> Parser.Parser a -> Expectation
parse cs =
    Expect.all <|
        List.map (\( s, x ) p -> Expect.equal (Ok x) (Parser.run p s)) cs


(=>) a b =
    ( a, b )


nestedList : Test
nestedList =
    describe "nested list parsing"
        [ test "basic tlist" <|
            \_ ->
                parse
                    [ "()" => Ts []
                    , "(word word word)" => Ts [ W, W, W ]
                    ]
                    tlist
        , test "nested tlist" <|
            \_ ->
                parse
                    [ "(word (word))" => Ts [ W, Ts [ W ] ]
                    , "(())" => Ts [ Ts [] ]
                    , "(()())" => Ts [ Ts [], Ts [] ]
                    , "(()word)" => Ts [ Ts [], W ]
                    , "(()()word (word))" => Ts [ Ts [], Ts [], W, Ts [ W ] ]
                    ]
                    tlist
        ]


suite : Test
suite =
    describe "module Parsers"
        [ nestedList
        , describe "basic parsers"
            [ test "parses a known integer" <|
                \_ ->
                    Expect.equal
                        (Ok (Int 42))
                        (Parser.run Parse.value "42")
            , fuzz int "parses a random integer" <|
                \i ->
                    Expect.equal
                        (Ok (Int i))
                        (Parser.run Parse.value (toString i))
            , test "parse a known string" <|
                \_ ->
                    Expect.equal
                        (Ok (String "a string\twith\\escape\"'s"))
                        (Parser.run Parse.value "\"a string\\twith\\\\escape\\\"'s\"")
            , test "parse a known bool" <|
                \_ ->
                    Expect.equal
                        (Ok (Bool True))
                        (Parser.run Parse.value "true")
            , test "parse an empty list" <|
                \_ ->
                    Expect.equal
                        (Ok (List []))
                        (Parser.run Parse.value "()")
            , test "parse another empty list" <|
                \_ ->
                    Expect.equal
                        (Ok (List []))
                        (Parser.run Parse.value "( ,, )")
            , test "parse a one-element list" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil ]))
                        (Parser.run Parse.value "(nil)")
            , test "parse a one-element list with leading space" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil ]))
                        (Parser.run Parse.value "( nil)")
            , test "parse a one-element list with trailing space" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil ]))
                        (Parser.run Parse.value "(nil )")
            , test "parse a one-element list with spaces" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil ]))
                        (Parser.run Parse.value "( nil )")
            , test "parse a straight list of nils" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil, Nil, Nil, Nil ]))
                        (Parser.run Parse.value "(nil nil nil nil)")
            , test "parse a spaced list of nils" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Nil, Nil, Nil, Nil ]))
                        (Parser.run Parse.value "( , nil,nil   nil,, nil )")
            , test "parse a simple list" <|
                \_ ->
                    Expect.equal
                        (Ok (List [ Bool False, String "hello, world", Int -15 ]))
                        (Parser.run Parse.value """(false "hello, world",  -15,)""")
            , test "nesting things" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Vector
                                [ List [ Int 1, String "yo" ]
                                , Nil
                                , Map [ ( List [ Bool True ], String "who" ), ( Nil, Int -3 ) ]
                                ]
                            )
                        )
                        (Parser.run Parse.value """[ (1, "yo"), nil, {(true) "who", nil -3} ]""")
            , skip <|
                test "list of empty maps" <|
                    \_ ->
                        Expect.equal
                            (Ok (List [ Map [], Map [], Map [] ]))
                            (Parser.run Parse.value "({}{}{})")
            , skip <|
                test "triples event" <|
                    \_ ->
                        Expect.equal
                            (Ok
                                (Map
                                    [ ( Symbol "cols", Int 4 )
                                    , ( Symbol "rows", Int 3 )
                                    , ( Symbol "deckSize", Int 0 )
                                    , ( Symbol "cards", Map [] )
                                    , ( Symbol "scores"
                                      , Map
                                            [ ( Symbol "match", Int 0 )
                                            , ( Symbol "matchWrong", Int 0 )
                                            , ( Symbol "noMatch", Int 0 )
                                            , ( Symbol "noMatchWrong", Int 0 )
                                            ]
                                      )
                                    ]
                                )
                            )
                            (Parser.run Parse.value """{:cols 4 :rows 3 :matchSize 3 :deckSize 0 :cards{}:scores{"Rob"{:match 0 :matchWrong 0 :noMatch 0 :noMatchWrong 0}}}
""")
            ]
        ]
