# De- and encoding of EDN

[EDN](https://github.com/edn-format/edn) (Extensible Data Notation) is a 
Clojure-derived data transfer format. This project aims to provide EDN 
decoders and encoders for Elm.

The `Decode` and `Encode` modules are modeled on the standard library's 
[`Json.Decode`][1] and [`Json.Encode`][2]


## Examples

First two EDN messages, as used in [siren](https://github.com/alicebob/siren).

```
statusMsg = """
#siren/status             ; a tag, applied to the following map
    { :state "play"       ; (keyword, string) map entry
    , :elapsed 11.342     ; a floating point number
    , :volume 30          ; an integer
    }
"""

playlistMsg = """
; compact message, two-element list
#siren/playlist({:pos 0 :track"01.mp3"}{:pos 1 :track"02.mp3"})
"""
```


You can write a decoder just like you would with `Json.Decode`.

```
import Decode

type alias Status =
    { state : String
    , elapsed : Float
    , volume : Int
    }

type alias PlaylistTrack =
    { pos : Int
    , track : String
    }

type Message =
    = MsgStatus Status
    | MsgPlaylist (List PlaylistTrack)

msgDecoder : Decode.Decoder Message
msgDecoder = Decode.tagged
    [ ( "siren/status"
      , Decode.map MsgStatus <|
          Decode.map3 Status
              (Decode.field "state" Decode.string)
              (Decode.field "elapsed" Decode.float)
              (Decode.field "volume" Decode.int)
      )
    , ( "siren/playlist"
      , Decode.map MsgPlaylist <|
          Decode.list <|
              Decode.map2 PlaylistTrack
                  (Decode.field "pos" Decode.int)
                  (Decode.field "track" Decode.string)
      )
    ]

Decode.decodeString msgDecoder statusMsg
--> Ok (MsgStatus {state = "play", elapsed = 11.342, volume = 30})
```

Or encode much like `Json.Encode`:

```
encodeMsg : Message -> Encode.Element
encodeMsg msg = case msg of
    MsgStatus status ->
        Encode.mustTagged "siren/status" <|
            Encode.mustObject
                [ ( "state", Encode.string status.state )
                , ( "elapsed", Encode.float status.elapsed )
                , ( "volume", Encode.int status.volume )
                ]

    MsgPlaylist tracks ->
        let
            encodeTrack track = Encode.mustObject
                [ ( "pos", Encode.int track.pos )
                , ( "track", Encode.string track.track )
                ]
        in
        Encode.mustTagged "siren/playlist" <|
            Encode.list (List.map encodeTrack tracks)

Encode.encode <| encodeMsg <| MsgStatus
    { state = "pause"
    , elapsed = 0
    , volume = 55
    }
--> "#siren/status {:state "pause", :elapsed 0.0, :volume 55}"
```


## Status

It's all still a bit rough around the edges, but the parsing and 
decoding should be mostly complete. Some element types are parsed 
correctly but not yet exposed through `Decode`, e.g. arbitrary precision 
numbers.

The encoding module is very much minimal effort so far, falling back to 
possibly incorrect string and number formatting primitives.


[1]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
[2]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode
