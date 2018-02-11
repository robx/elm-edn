module Thing exposing (Thing(..), thing)

import List
import Parser as P exposing ((|.), (|=), Parser)


type Thing
    = Number Int
    | Things (List Thing)


thing : Parser Thing
thing =
    P.oneOf
        [ number
        , P.lazy (\_ -> things)
        ]


number : Parser Thing
number =
    P.succeed Number
        |= P.int
        |. sep


things : Parser Thing
things =
    let
        spaceOrThing =
            P.oneOf
                [ P.succeed Nothing |. space
                , P.succeed Just |= P.lazy (\_ -> thing)
                ]
    in
    P.succeed (Things << List.filterMap identity)
        |. P.symbol "("
        |= P.repeat P.zeroOrMore spaceOrThing
        |. P.symbol ")"


sep : Parser ()
sep =
    P.oneOf
        [ space
        , P.end
        , P.lookAhead <|
            P.oneOf
                [ P.symbol "("
                , P.symbol ")"
                ]
        ]


space : Parser ()
space =
    P.ignore P.oneOrMore (\c -> c == ' ')
