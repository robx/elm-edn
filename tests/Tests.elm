module Tests exposing (..)

import Date
import Decode
import Dict
import Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser exposing ((|.))
import Test exposing (..)
import Thing as T
import Types exposing (..)


toMap : List ( Element, Element ) -> Element
toMap l =
    Map Dict.empty l


parse : List ( String, a ) -> Parser.Parser a -> Expectation
parse cs =
    Expect.all <|
        List.map (\( s, x ) p -> Expect.equal (Ok x) (Parser.run p s)) cs


parseOk : List ( String, Bool ) -> Parser.Parser a -> Expectation
parseOk cs =
    let
        ok r =
            case r of
                Ok _ ->
                    True

                Err _ ->
                    False
    in
    Expect.all <|
        List.map (\( s, x ) p -> Expect.equal x (ok <| Parser.run p s)) cs


(=>) a b =
    ( a, b )


element =
    Parse.onlyElement


elements =
    Parse.onlyElements


decode : Decode.Decoder a -> List ( String, a ) -> Expectation
decode dec cs =
    Expect.all
        (List.map (\( s, x ) d -> Expect.equal (Ok x) (Decode.decodeString d s)) cs)
        dec


suite : Test
suite =
    describe "EDN"
        [ describe "module Parsers"
            [ test "basic list" <|
                \_ ->
                    parse
                        [ "()" => List []
                        , "(nil nil nil)" => List [ Nil, Nil, Nil ]
                        ]
                        element
            , test "nested list" <|
                \_ ->
                    parse
                        [ "(nil (nil))" => List [ Nil, List [ Nil ] ]
                        , "(())" => List [ List [] ]
                        , "(()())" => List [ List [], List [] ]
                        , "(()nil)" => List [ List [], Nil ]
                        , "(()()nil (nil))" => List [ List [], List [], Nil, List [ Nil ] ]
                        ]
                        element
            , test "sample" <|
                \_ ->
                    parse
                        [ "(12 (34))" => T.Things [ T.Number 12, T.Things [ T.Number 34 ] ]
                        , "(())" => T.Things [ T.Things [] ]
                        , "(()())" => T.Things [ T.Things [], T.Things [] ]
                        , "(()44)" => T.Things [ T.Things [], T.Number 44 ]
                        , "(()()123 (456))" => T.Things [ T.Things [], T.Things [], T.Number 123, T.Things [ T.Number 456 ] ]
                        , "( 11 22  33 )" => T.Things [ T.Number 11, T.Number 22, T.Number 33 ]
                        , "( 11 ( 22 )(  33) )" => T.Things [ T.Number 11, T.Things [ T.Number 22 ], T.Things [ T.Number 33 ] ]
                        , "(1()2)" => T.Things [ T.Number 1, T.Things [], T.Number 2 ]
                        ]
                        T.thing
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
                        , "trueorfalse" => Symbol "trueorfalse"
                        , "true#_#_#" => Symbol "true#_#_#"
                        , "(yo1)"
                            => List [ Symbol "yo1" ]
                        , """("yo"1)"""
                            => List [ String "yo", Int 1 ]
                        , """("yo":yo)"""
                            => List [ String "yo", Keyword "yo" ]
                        , """#{this is a set}"""
                            => Set [ Symbol "this", Symbol "is", Symbol "a", Symbol "set" ]
                        , """:a-keyword-with-dashes"""
                            => Keyword "a-keyword-with-dashes"
                        , """ "some;line%%%noise" """
                            => String "some;line%%%noise"
                        , """ "me & you" """
                            => String "me & you"
                        , """ "me & you \\u0026 them" """
                            => String "me & you & them"
                        ]
                        element
            , test "chars" <|
                \_ ->
                    parse
                        [ """\\a"""
                            => Char 'a'
                        , """\\\\"""
                            => Char '\\'
                        , """\\newline"""
                            => Char '\n'
                        , """\\u0026"""
                            => Char '&'
                        ]
                        element
            , fuzz int "parses a random integer" <|
                \i ->
                    Expect.equal
                        (Ok (Int i))
                        (Parser.run element (toString i))
            , test "nesting things" <|
                \_ ->
                    parse
                        [ "[()nil]"
                            => Vector [ List [], Nil ]
                        , "(() ())"
                            => List [ List [], List [] ]
                        , "(() nil)"
                            => List [ List [], Nil ]
                        , "[() nil]"
                            => Vector [ List [], Nil ]
                        , "[ (), nil ]"
                            => Vector [ List [], Nil ]
                        , """[ (1, "yo"), nil, {(true) "who", nil -3} ]"""
                            => Vector
                                [ List [ Int 1, String "yo" ]
                                , Nil
                                , toMap [ ( List [ Bool True ], String "who" ), ( Nil, Int -3 ) ]
                                ]
                        , "({}{}{})" => List [ toMap [], toMap [], toMap [] ]
                        ]
                        element
            , test "tags" <|
                \_ ->
                    parse
                        [ "#type nil"
                            => Tagged "type" Nil
                        , "#myapp/type nil"
                            => Tagged "myapp/type" Nil
                        , "#tag()"
                            => Tagged "tag" (List [])
                        , "#tag ()"
                            => Tagged "tag" (List [])
                        , "(#tag (nil)#tug nil)"
                            => List [ Tagged "tag" (List [ Nil ]), Tagged "tug" Nil ]
                        , "#my/tag[1,2,3]"
                            => Tagged "my/tag" (Vector [ Int 1, Int 2, Int 3 ])
                        ]
                        element
            , test "triples" <|
                \_ ->
                    parse
                        [ """{:cols 4 :rows 3 :matchSize 3 :deckSize 0 :cards{}:scores{"Rob"{:match 0 :matchWrong 0 :noMatch 0 :noMatchWrong 0}}}
"""
                            => Map
                                (Dict.fromList
                                    [ ( "cols", Int 4 )
                                    , ( "rows", Int 3 )
                                    , ( "matchSize", Int 3 )
                                    , ( "deckSize", Int 0 )
                                    , ( "cards", toMap [] )
                                    , ( "scores"
                                      , toMap
                                            [ ( String "Rob"
                                              , Map
                                                    (Dict.fromList
                                                        [ ( "match", Int 0 )
                                                        , ( "matchWrong", Int 0 )
                                                        , ( "noMatch", Int 0 )
                                                        , ( "noMatchWrong", Int 0 )
                                                        ]
                                                    )
                                                    []
                                              )
                                            ]
                                      )
                                    ]
                                )
                                []
                        , """{:name"Rob"}"""
                            => Map (Dict.fromList [ ( "name", String "Rob" ) ]) []
                        , """#triples/eventClaimed{:name"Liz":type"match":result"wrong":score{ :match 0 :matchWrong 1 :noMatch 0 :noMatchWrong 1}}"""
                            => Tagged "triples/eventClaimed"
                                (Map
                                    (Dict.fromList
                                        [ ( "name", String "Liz" )
                                        , ( "type", String "match" )
                                        , ( "result", String "wrong" )
                                        , ( "score"
                                          , Map
                                                (Dict.fromList
                                                    [ ( "match", Int 0 )
                                                    , ( "matchWrong", Int 1 )
                                                    , ( "noMatch", Int 0 )
                                                    , ( "noMatchWrong", Int 1 )
                                                    ]
                                                )
                                                []
                                          )
                                        ]
                                    )
                                    []
                                )
                        ]
                        element
            , test "discard" <|
                \_ ->
                    parse
                        [ "(1 #_ #_ 2 3 4)"
                            => List [ Int 1, Int 4 ]
                        , "#_nil nil"
                            => Nil
                        , "#_nil #_nil nil"
                            => Nil
                        , "(#_ #tag 3, xxx)"
                            => List [ Symbol "xxx" ]
                        , "(#_ nil)"
                            => List []
                        , "(#_nil)"
                            => List []
                        , "(#_#my/tag[2,3,4])"
                            => List []
                        , "(#_#my/tag[2,3,4]#_nil,true#_,#_false)"
                            => List [ Symbol "true#_" ]
                        , "#_ #zap #_ xyz foo bar"
                            => Symbol "bar"
                        , "#_ #foo #foo #foo #_#_bar baz zip quux"
                            => Symbol "quux"
                        ]
                        element
            , test "looooooong list" <|
                -- seems to scale linearly, ran up to 100k
                let
                    longList =
                        "(" ++ String.repeat 1000 "()" ++ ")"
                in
                \_ ->
                    case Parser.run element longList of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail (toString err)
            , test "numbers" <|
                \_ ->
                    parse
                        [ "1." => Float 1.0
                        , "1.1" => Float 1.1
                        , "-1.1" => Float -1.1
                        , "1" => Int 1
                        , "1E1" => Float 10.0
                        , "1e1" => Float 10.0
                        , "1e-1" => Float 0.1
                        , "-1e-2" => Float -0.01
                        , "3.1415926" => Float 3.1415926
                        , "1.00000" => Float 1.0
                        , "-55" => Int -55
                        , "1.234E5M" => BigFloat { sign = "+", digits = "1" } "234" { sign = "+", digits = "5" }
                        , "0.0" => Float 0
                        , "0" => Int 0
                        , "0N" => BigInt { sign = "+", digits = "0" }
                        ]
                        element
            , test "comments" <|
                \_ ->
                    parse
                        [ "[];;and some more () fun things!!" => Vector []
                        , "(let us try ; nothing\nsomething interesting!)"
                            => List [ Symbol "let", Symbol "us", Symbol "try", Symbol "something", Symbol "interesting!" ]
                        , """("a string doesn't care; yeah?") ; "hello)"""
                            => List [ String "a string doesn't care; yeah?" ]
                        , """;; first a comment
(then a list)"""
                            => List [ Symbol "then", Symbol "a", Symbol "list" ]
                        ]
                        element
            , test "space" <|
                \_ ->
                    parse
                        [ " 1 ;" => Int 1
                        ]
                        element
            , test "elements" <|
                \_ ->
                    parse
                        [ " 1 ;" => [ Int 1 ]
                        , "1 1 1" => [ Int 1, Int 1, Int 1 ]
                        ]
                        elements
            , test "symbols" <|
                \_ ->
                    parseOk
                        [ "woooo" => True
                        , "/" => True
                        , "//" => False
                        , "12ab" => False
                        , ".abc" => True
                        , ".1abc" => False
                        , "a/b" => True
                        , "somethinglong/" => False
                        , "spaces are out" => False
                        , "more/than/one" => False
                        , "#nostarting" => False
                        , "butfine#" => True
                        , "notthesecond/.1" => False
                        ]
                        (Parse.plainSymbol |. Parser.end)
            ]
        , describe "module Decode" <|
            [ test "a simple string" <|
                \_ ->
                    decode Decode.string
                        [ "\"hello world\" "
                            => "hello world"
                        ]
            , test "#inst" <|
                let
                    expect =
                        case Date.fromString "1985-04-12T23:20:50.52Z" of
                            Ok d ->
                                d

                            Err e ->
                                Debug.crash <| "bad date: " ++ e
                in
                \_ ->
                    decode Decode.instant
                        [ "#inst \"1985-04-12T23:20:50.52Z\""
                            => expect
                        ]
            ]
        , describe "module Encode"
            [ test "plain symbol" <|
                \_ ->
                    Expect.notEqual
                        (Encode.toSymbol "cmd")
                        Nothing
            , test "mustSymbol" <|
                \_ ->
                    Expect.equal
                        ("cmd"
                            |> Encode.mustSymbol
                            |> Encode.symbol
                            |> Encode.encode
                        )
                        "cmd"
            , test "no spaces" <|
                \_ ->
                    Expect.equal
                        (Encode.toSymbol "a mess cmd")
                        Nothing
            , test "no nil/true/false" <|
                \_ ->
                    Expect.equal
                        (List.map Encode.toSymbol [ "nil", "true", "false" ])
                        [ Nothing, Nothing, Nothing ]
            ]
        ]
