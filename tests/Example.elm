module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "module Parsers"
        [ describe "basic parsers"
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
            ]
        ]
