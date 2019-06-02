module Base64.Decode exposing
    ( Decoder, string, bytes
    , decode, map, Error(..)
    )

{-| The Decode module enables you to decode Base64 strings into certain values.


# Decoders

@docs Decoder, string, bytes


# Decoding

@docs decode, map, Error

-}

import Base64.Shift as Shift exposing (Shift(..))
import Base64.Table as Table
import Bitwise
import Bytes exposing (Bytes(..))
import Bytes.Decode exposing (Step(..), loop)
import Bytes.Encode
import Char
import Dict exposing (Dict)
import Regex exposing (Regex)


{-| A decoder type that knows how to decode a Base64 string.
-}
type Decoder a
    = Decoder (String -> Result Error a)


{-| Bytes `Decoder`.
-}
bytes : Decoder Bytes
bytes =
    Decoder
        (\input ->
            case tryDecode input of
                Ok ( _, _, deferredEncoders ) ->
                    Ok (encodeBytes deferredEncoders)

                Err e ->
                    -- Propagated ValidationError
                    Err e
        )


{-| String `Decoder`.
-}
string : Decoder String
string =
    Decoder
        (\input ->
            case tryDecode input of
                Ok ( _, _, deferredEncoders ) ->
                    deferredEncoders
                        |> encodeBytes
                        |> tryToString

                Err e ->
                    -- Propagated ValidationError
                    Err e
        )


{-| Turn a `Decoder` into a certain value.

Decoding can fail, see `Error`.

-}
decode : Decoder a -> String -> Result Error a
decode (Decoder decoder) input =
    decoder input


{-| Transforms the value produced by a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map transform (Decoder decoder_) =
    Decoder (decoder_ >> Result.map transform)


{-| Errors that can occur during a decoding:

    Base64.Decode.decode Base64.Decode.string "a===" == Err ValidationError

    Base64.Decode.decode Base64.Decode.string "/Ng9" == Err InvalidByteSequence

-}
type Error
    = ValidationError
    | InvalidByteSequence



-- Private


tryDecode : String -> Result Error DecodeState
tryDecode input =
    strip input
        |> Result.andThen validate
        |> Result.map
            (String.foldl
                (\char state ->
                    Table.decode char
                        |> Maybe.map (\sextet -> decodeStep sextet state)
                        |> Maybe.withDefault state
                )
                initialState
            )


{-| The meat structure of the decoding. It uses a list of encoders that later be transformed in
the target byte sequence. The byte operations are deferred for the sake of perforamnce.
-}
type alias DecodeState =
    ( Shift, Int, List Bytes.Encode.Encoder )


initialState : DecodeState
initialState =
    ( Shift0, 0, [] )


encodeBytes : List Bytes.Encode.Encoder -> Bytes
encodeBytes encoders =
    List.reverse encoders
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


decodeStep : Int -> DecodeState -> DecodeState
decodeStep sextet ( shift, partialByte, deferredEncoders ) =
    let
        finishedByte =
            case shift of
                Shift0 ->
                    Nothing

                Shift2 ->
                    Just (finishPartialByte Shift4 sextet partialByte)

                Shift4 ->
                    Just (finishPartialByte Shift2 sextet partialByte)

                Shift6 ->
                    Just (Bitwise.or partialByte sextet)

        nextDeferredDecoders =
            case finishedByte of
                Just byte_ ->
                    Bytes.Encode.unsignedInt8 byte_ :: deferredEncoders

                Nothing ->
                    deferredEncoders

        nextBlankByte =
            case shift of
                Shift0 ->
                    Shift.shiftLeftBy Shift2 sextet

                Shift2 ->
                    Shift.shiftLeftBy Shift4 sextet

                Shift4 ->
                    Shift.shiftLeftBy Shift6 sextet

                Shift6 ->
                    0
    in
    ( Shift.decodeNext shift
    , nextBlankByte
    , nextDeferredDecoders
    )


finishPartialByte : Shift -> Int -> Int -> Int
finishPartialByte shift sextet partialByte =
    Bitwise.or partialByte (Shift.shiftRightZfBy shift sextet)



-- Input validation


strip : String -> Result a String
strip input =
    if String.endsWith "==" input then
        Ok (String.dropRight 2 input)

    else if String.endsWith "=" input then
        Ok (String.dropRight 1 input)

    else
        Ok input


validate : String -> Result Error String
validate input =
    let
        regex =
            Regex.fromString "^[A-Za-z0-9\\/+]*$"
                |> Maybe.withDefault Regex.never
    in
    if Regex.contains regex input then
        Ok input

    else
        Err ValidationError



-- Bytes helpers


tryToString : Bytes -> Result Error String
tryToString input =
    case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width input)) input of
        Just str ->
            Ok str

        Nothing ->
            Err InvalidByteSequence
