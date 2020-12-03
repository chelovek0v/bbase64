# Base64

This package helps you work with Base64 encoding, providing a similar interface as in [elm/bytes](https://package.elm-lang.org/packages/elm/bytes/latest) and [elm/json](https://package.elm-lang.org/packages/elm/json/latest)

**THIS PACKAGE HAS BEEN RENAMED. PREVIOUS NAME was ivadzy/bbase65**

Please update your dependencies, pay attention to the package version because it was reset to 1.0.0 from 1.1.1.

## Why another Base64 library?

* Supports Elm 0.19
* Insensitive to padding (as atob/btoa in JS)
* Functional implementation
* Supports raw bytes encoding/decoding
* There is elm/bytes under the hood, so less errors will occur during validations
* Interface is consistent with an Elm's encoders/decoders interface

## Usage

The package lets you create encoders and decoders to work with strings and bytes. 

Converting a string into a Base64 string:

```elm
import Bytes.Encode
import Base64.Encode as Encode


input =
    "Example input string"

encoded =
    Encode.encode (Encode.string input)

```

Encoding a bytes seqeunce:

```elm
import Bytes.Encode
import Base64.Decode as Decode
import Base64.Encode as Encode

stringAsBytes = 
    Bytes.Encode.encode (Bytes.Encode.string "H💩la!")

encoded =
    Encode.encode (Encode.bytes stringAsBytes)

decoded =
    Decode.decode Decode.string encoded
```

Or you can decode string into raw bytes with `Base64.Decode.bytes`:

```elm
import Base64.Decode as Decode
import Bytes
import Bytes.Decode

-- decodedAsBytes = Decode.decode Decode.bytes someInput
decodedAsString =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width decodedAsBytes)) decodedAsBytes

```

Also you can map `Decoder`:

```elm
import Base64.Decode as Decode

input =
    "TWFu"

decoder =
    Decode.map String.reverse Decode.string

decoded = 
    Decode.decode decoder input -- naM
```

