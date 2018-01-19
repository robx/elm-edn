module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser
import Test exposing (..)
import Types


suite : Test
suite =
    describe "module Parsers"
        [ describe "basic parsers"
            [ test "parses a known integer" <|
                \_ ->
                    Expect.equal
                        (Ok (Types.Int 42))
                        (Parser.run Parse.value "42")
            , fuzz int "parses a random integer" <|
                \i ->
                    Expect.equal
                        (Ok (Types.Int i))
                        (Parser.run Parse.value (toString i))
            , test "parse a known string" <|
                \_ ->
                    Expect.equal
                        (Ok (Types.String "a string\twith\\escape\"'s"))
                        (Parser.run Parse.value "\"a string\\twith\\\\escape\\\"'s\"")
            , test "parse a known bool" <|
                \_ ->
                    Expect.equal
                        (Ok (Types.Bool True))
                        (Parser.run Parse.value "true")
            ]
        ]
