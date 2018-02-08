# De- and encoding of EDN (extensible data notation)

[edn](https://github.com/edn-format/edn) is a Clojure-derived
data transfer format. This project aims to provide EDN decoders
and encoders for Elm.

The `Decode` and `Encode` modules are modeled on the standard
library's [`Json.Decode`][1] and [`Json.Encode`][2]

## Status

It's all still a bit rough around the edges, but the parsing and decoding
should be mostly complete. Some element types are parsed correctly but
not yet exposed through `Decode`, e.g. arbitrary precision numbers and
characters.

The encoding module is very much minimal effort so far, falling back
to possibly incorrect string and number formatting primitives.


[1]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
[2]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode
