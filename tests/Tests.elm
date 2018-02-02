module Tests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parse
import Parser exposing ((|.))
import Test exposing (..)
import Types exposing (..)


toMap : List ( Element, Element ) -> Element
toMap l =
    Map Dict.empty l


parse : List ( String, a ) -> Parser.Parser a -> Expectation
parse cs =
    Expect.all <|
        List.map (\( s, x ) p -> Expect.equal (Ok x) (Parser.run p s)) cs


(=>) a b =
    ( a, b )


element =
    Parse.onlyElement


elements =
    Parse.onlyElements


suite : Test
suite =
    describe "module Parsers"
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
                    [ "1" => Int 1
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
        , test "elemenets" <|
            \_ ->
                parse
                    [ " 1 ;" => [ Int 1 ]
                    , "1 1 1" => [ Int 1, Int 1, Int 1 ]
                    ]
                    elements
        , test "config" <|
            \_ ->
                case Parser.run element config of
                    Ok _ ->
                        Expect.pass

                    Err err ->
                        Expect.fail (toString err)
        ]


config : String
config =
    """
;; Sample configuration for myapp.

{;; The db map is the set of values required to login to the postgres database
 ;; we connect to.
 :db {:user "my-username"
      :pwd "secret"
      :host "hostname.at.my-region.rds.amazonaws.com"
      :db "databasename"
      :port 5432}
 ;; Configuration options for myapp
 :myapp {;; Myapp is really available on 443 through reverse proxying done by
         ;; nginx, to avoid handling SSL ourselves. 3000 is blocked to the
         ;; public via iptables.
         :port 3000
         ;; The features are allowed on the form #{:foo :bar :baz}, but the
         ;; common form :all is better when you want all enabled. All options
         ;; available are: :admin-panel, :swear-filter, :ads,
         ;; :keyboard-shortcuts and :pirate-lang. See the internal wiki page for
         ;; details.
         :features #{:admin-panel :keyboard-shortcuts} #_:all
         ;; Configuration for the foo service which we depend on
         :foo {;; The DNS entry to lookup to connect to a foo service. If you
               ;; use the DNS to a specific cluster -- like "eu1.foo.mycorp.com"
               ;; -- you only have to provide that key.
               :hostname "foo.mycorp.com"
               ;; Keys to the foo service. Starts with key1, goes on to key2 if
               ;; that fails and so on. We'd like it to be a single key some
               ;; day, but unfortunately we opened the foo API to some clients.
               ;; As a stupid way to handle rate limiting we decided that it
               ;; would be a good idea to use different keys for different
               ;; clusters, instead of giving specific users specific keys
               ;; instead.
               :api-keys ["key1" "key2"]
               ;; How often we check for more data from foo. It's recommended to
               ;; turn this down to 20 minutes, because a lot of instances of
               ;; bar would end up rechecking myapp waiting for data. Basically
               ;; what caused the major outage earlier this year.
               :recheck-frequency #duration "20m"}
         ;; Timestamp to put on elements that will be cached forever by HTTP
         ;; caches. If not set, it is placed one year ahead of the current time.
         :forever-date #inst "2032-01-01T12:20:50.52Z"
         ;; How many goroutines we should delegate to processing data.
         :process-pool 5}
 ;; The loglevel. Use :warn only if the logs become too verbose or you don't
 ;; need the data, otherwise use :info. Use :debug only in development
 ;; environments because that thing spits out basically everything.
 :log :debug
 ;; Which environment we're in. Has nothing to say for the app itself, but it's
 ;; attached on the log messages sent to our centralised logging system.
:env :development}
"""
