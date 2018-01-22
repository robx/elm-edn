module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser exposing ((|.))
import Test exposing (..)
import Types exposing (..)


parse : List ( String, a ) -> Parser.Parser a -> Expectation
parse cs =
    Expect.all <|
        List.map (\( s, x ) p -> Expect.equal (Ok x) (Parser.run p s)) cs


(=>) a b =
    ( a, b )


nestedList : Test
nestedList =
    describe "nested list parsing"
        [ test "basic list" <|
            \_ ->
                parse
                    [ "()" => List []
                    , "(nil nil nil)" => List [ Nil, Nil, Nil ]
                    ]
                    Parse.element
        , test "nested list" <|
            \_ ->
                parse
                    [ "(nil (nil))" => List [ Nil, List [ Nil ] ]
                    , "(())" => List [ List [] ]
                    , "(()())" => List [ List [], List [] ]
                    , "(()nil)" => List [ List [], Nil ]
                    , "(()()nil (nil))" => List [ List [], List [], Nil, List [ Nil ] ]
                    ]
                    Parse.element
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
                    , "trueorfalse" => Symbol "trueorfalse"
                    , "true#_#_#" => Symbol "true#_#_#"
                    ]
                    Parse.element
        , fuzz int "parses a random integer" <|
            \i ->
                Expect.equal
                    (Ok (Int i))
                    (Parser.run Parse.element (toString i))
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
                            , Map [ ( List [ Bool True ], String "who" ), ( Nil, Int -3 ) ]
                            ]
                    , "({}{}{})" => List [ Map [], Map [], Map [] ]
                    ]
                    Parse.element
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
                    Parse.element
        , test "triples" <|
            \_ ->
                parse
                    [ """{:cols 4 :rows 3 :matchSize 3 :deckSize 0 :cards{}:scores{"Rob"{:match 0 :matchWrong 0 :noMatch 0 :noMatchWrong 0}}}
"""
                        => Map
                            [ ( Keyword "cols", Int 4 )
                            , ( Keyword "rows", Int 3 )
                            , ( Keyword "matchSize", Int 3 )
                            , ( Keyword "deckSize", Int 0 )
                            , ( Keyword "cards", Map [] )
                            , ( Keyword "scores"
                              , Map
                                    [ ( String "Rob"
                                      , Map
                                            [ ( Keyword "match", Int 0 )
                                            , ( Keyword "matchWrong", Int 0 )
                                            , ( Keyword "noMatch", Int 0 )
                                            , ( Keyword "noMatchWrong", Int 0 )
                                            ]
                                      )
                                    ]
                              )
                            ]
                    , """{:name"Rob"}"""
                        => Map [ ( Keyword "name", String "Rob" ) ]
                    ]
                    Parse.element
        , test "discard" <|
            \_ ->
                parse
                    [ "(1 #_ #_ 2 3 4)"
                        => List [ Int 1, Int 4 ]
                    , "#_nil nil"
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
                    Parse.element
        ]
