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
        , test "basic parsers" <|
            \_ ->
                parse
                    [ "42" => Int 42
                    , "\"a string\\twith\\\\escape\\\"'s\""
                        => String "a string\twith\\escape\"'s"
                    , "true" => Bool True
                    , "()" => List []
                    , "( ,, )" => List []
                    , "(nil)" => List [ Nil ]
                    , "( nil)" => List [ Nil ]
                    , "(nil )" => List [ Nil ]
                    , "( nil )" => List [ Nil ]
                    , "(nil nil nil nil)" => List [ Nil, Nil, Nil, Nil ]
                    , "( , nil,nil   nil,, nil )" => List [ Nil, Nil, Nil, Nil ]
                    , """(false "hello, world",  -15,)"""
                        => List [ Bool False, String "hello, world", Int -15 ]
                    ]
                    Parse.value
        , fuzz int "parses a random integer" <|
            \i ->
                Expect.equal
                    (Ok (Int i))
                    (Parser.run Parse.value (toString i))
        , test "nesting things" <|
            \_ ->
                parse
                    [ """[ (1, "yo"), nil, {(true) "who", nil -3} ]"""
                        => Vector
                            [ List [ Int 1, String "yo" ]
                            , Nil
                            , Map [ ( List [ Bool True ], String "who" ), ( Nil, Int -3 ) ]
                            ]
                    , "({}{}{})" => List [ Map [], Map [], Map [] ]
                    ]
                    Parse.value
        , test "triples" <|
            \_ ->
                parse
                    [ """{:cols 4 :rows 3 :matchSize 3 :deckSize 0 :cards{}:scores{"Rob"{:match 0 :matchWrong 0 :noMatch 0 :noMatchWrong 0}}}
"""
                        => Map
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
                    ]
                    Parse.value
        ]
