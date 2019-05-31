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
                Ok state ->
                    Ok <| wrapUp state

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
                Ok state ->
                    state |> wrapUp |> tryToString

                Err e ->
                    -- Propagated ValidationError
                    Err e
        )


wrapUp : DecodeState -> Bytes
wrapUp ( _, _, encoders ) =
    List.reverse encoders
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


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
                (\c state ->
                    Table.decode c
                        |> Maybe.map (\i -> decodeStep i state)
                        |> Maybe.withDefault state
                )
                initialState
            )


type alias DecodeState =
    ( Shift, Int, List Bytes.Encode.Encoder )


initialState : DecodeState
initialState =
    ( Shift0, 0, [] )


decodeStep : Int -> DecodeState -> DecodeState
decodeStep sixtet ( shift, blankByte, bytes_ ) =
    let
        maybeFinishedByte =
            case shift of
                Shift0 ->
                    Nothing

                Shift2 ->
                    Just (finishBlankByte Shift4 sixtet blankByte)

                Shift4 ->
                    Just (finishBlankByte Shift2 sixtet blankByte)

                Shift6 ->
                    Just (Bitwise.or blankByte sixtet)

        nextBytes =
            case maybeFinishedByte of
                Just finishedByte ->
                    Bytes.Encode.unsignedInt8 finishedByte :: bytes_

                Nothing ->
                    bytes_

        nextBlankByte =
            case shift of
                Shift0 ->
                    Shift.shiftLeftBy Shift2 sixtet

                Shift2 ->
                    Shift.shiftLeftBy Shift4 sixtet

                Shift4 ->
                    Shift.shiftLeftBy Shift6 sixtet

                Shift6 ->
                    0
    in
    ( Shift.decodeNext shift
    , nextBlankByte
    , nextBytes
    )


finishBlankByte : Shift -> Int -> Int -> Int
finishBlankByte shift sixtet blankByte =
    Bitwise.or blankByte (Shift.shiftRightZfBy shift sixtet)



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
