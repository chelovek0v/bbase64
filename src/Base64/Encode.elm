module Base64.Encode exposing
    ( Encoder, string, bytes
    , encode
    )

{-| The Encode module enables you to encode strings and raw bytes.


# Encoders

@docs Encoder, string, bytes


# Encoding

@docs encode

-}

import Base64.Shift as Shift exposing (Shift(..))
import Base64.Table as Table
import Bitwise
import Bytes exposing (Bytes(..))
import Bytes.Decode exposing (Step(..), loop)
import Bytes.Encode
import Char
import Dict exposing (Dict)


{-| A type that knows how to encode certain data into a
Base64 string.
-}
type Encoder
    = StringEncoder String
    | BytesEncoder Bytes


{-| String `Encoder`.
-}
string : String -> Encoder
string input =
    StringEncoder input


{-| Bytes `Encoder`.
-}
bytes : Bytes -> Encoder
bytes input =
    BytesEncoder input


{-| Turns an `Encoder` into a Base64 string.
-}
encode : Encoder -> String
encode encoder =
    case encoder of
        StringEncoder input ->
            Bytes.Encode.encode (Bytes.Encode.string input)
                |> tryEncode
                |> Maybe.withDefault ""

        BytesEncoder input ->
            tryEncode input
                |> Maybe.withDefault ""



-- Private


tryEncode : Bytes -> Maybe String
tryEncode input =
    let
        decoderInitialState =
            ( Bytes.width input, initialEncodeState )

        base64Decoder =
            loop decoderInitialState (decodeStep Bytes.Decode.unsignedInt8)
    in
    -- Encoding through decoding :--)
    input
        |> Bytes.Decode.decode base64Decoder
        |> Maybe.map finalize


decodeStep : Bytes.Decode.Decoder Int -> DecodeState -> Bytes.Decode.Decoder (Step DecodeState EncodeState)
decodeStep octetDecoder ( n, encodeState ) =
    if n <= 0 then
        Bytes.Decode.succeed (Done encodeState)

    else
        Bytes.Decode.map (\octet -> Loop ( n - 1, encodeStep octet encodeState )) octetDecoder


type alias DecodeState =
    ( Int, EncodeState )


type alias EncodeState =
    ( Shift, Int, String )


initialEncodeState : EncodeState
initialEncodeState =
    ( Shift0
    , 0
    , ""
    )


encodeStep : Int -> EncodeState -> EncodeState
encodeStep octet (( shift, _, strAcc ) as encodeState) =
    let
        currentSixtet =
            sixtet octet encodeState

        base64Char =
            case shift of
                Shift6 ->
                    Table.encode currentSixtet
                        ++ Table.encode (Shift.shiftRightZfBy Shift2 octet)

                _ ->
                    Table.encode currentSixtet

        nextSixtet =
            case shift of
                Shift0 ->
                    Shift.and Shift2 octet

                Shift2 ->
                    Shift.and Shift4 octet

                Shift4 ->
                    Shift.and Shift6 octet

                Shift6 ->
                    Shift.and Shift2 octet
    in
    ( Shift.next shift
    , nextSixtet
    , strAcc ++ base64Char
    )


sixtet : Int -> EncodeState -> Int
sixtet octet ( shift, sixtet_, strAcc ) =
    case shift of
        Shift0 ->
            Shift.shiftRightZfBy Shift2 octet

        Shift2 ->
            Bitwise.or (Shift.shiftLeftBy Shift4 sixtet_) <|
                Shift.shiftRightZfBy Shift4 octet

        Shift4 ->
            Bitwise.or (Shift.shiftLeftBy Shift2 sixtet_) <|
                Shift.shiftRightZfBy Shift6 octet

        Shift6 ->
            sixtet_


finalize : EncodeState -> String
finalize ( shift, sixtet_, strAcc ) =
    case shift of
        Shift6 ->
            strAcc
                ++ Table.encode sixtet_

        Shift4 ->
            strAcc
                ++ Table.encode (Shift.shiftLeftBy Shift2 sixtet_)
                ++ "="

        Shift2 ->
            strAcc
                ++ Table.encode (Shift.shiftLeftBy Shift4 sixtet_)
                ++ "=="

        _ ->
            strAcc
