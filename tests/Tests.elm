module Tests exposing (badInputSuite, bytesDecodeSuite, bytesEncodeSuite, decodeSuite, encodeSuite, identitySuite)

import Base64.Decode as Decode
import Base64.Encode as Encode
import Bytes.Decode
import Bytes.Encode
import Expect exposing (Expectation)
import Fuzz exposing (string)
import Test exposing (..)



-- Inspired by https://github.com/truqu/elm-base64/tree/master/tests


cases : List ( String, String )
cases =
    [ ( "", "" )
    , ( "f", "Zg==" )
    , ( "fo", "Zm8=" )
    , ( "foo", "Zm9v" )
    , ( "foob", "Zm9vYg==" )
    , ( "fooba", "Zm9vYmE=" )
    , ( "foobar", "Zm9vYmFy" )
    , ( "\n", "Cg==" )
    , ( "\u{0000}", "AA==" )
    , ( "✓ à la mode", "4pyTIMOgIGxhIG1vZGU=" )
    , ( "💩", "8J+SqQ==" )
    , ( "💩💩💩", "8J+SqfCfkqnwn5Kp" )
    , ( "Man", "TWFu" )
    , ( String.repeat 5000 "Man", String.repeat 5000 "TWFu" )
    ]


encodeSuite : Test
encodeSuite =
    describe "Encode.encode (Encode.string ...)"
        (encodeHelper cases (Encode.encode << Encode.string))


decodeSuite : Test
decodeSuite =
    describe "Decode.decode Decode.string ..."
        (decodeHelper cases (Decode.decode Decode.string))


bytesDecodeSuite : Test
bytesDecodeSuite =
    describe "Decode.decode Decode.bytes ... must properly decode bytes sequence"
        [ test "Must decode < 000000000 >" <|
            \_ ->
                let
                    byte =
                        5

                    bytes =
                        Bytes.Encode.encode (Bytes.Encode.unsignedInt8 byte)

                    result =
                        Decode.decode Decode.bytes "BQ=="

                    unwrappedResult =
                        case result of
                            Ok bytes_ ->
                                Maybe.withDefault (byte + 1) (Bytes.Decode.decode Bytes.Decode.unsignedInt8 bytes_)

                            Err e ->
                                byte + 1
                in
                -- TODO: come up with a better idea how to test bytes
                -- convert to list -> byte to byte comparison
                Expect.equal byte unwrappedResult
        ]


bytesEncodeSuite : Test
bytesEncodeSuite =
    describe "Encode.encode (Encode.bytes ...) must properly encode bytes sequence"
        [ test "Must encode < 000000000 >" <|
            \_ ->
                let
                    bytes =
                        Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0)

                    result =
                        Encode.encode (Encode.bytes bytes)
                in
                Expect.equal "AA==" result
        ]


badInputSuite : Test
badInputSuite =
    let
        badInput =
            [ "a=aa"
            , "a==="
            ]

        validationErrorTests =
            badInput
                |> List.map
                    (\input ->
                        test ("Decode fails for " ++ input) <|
                            \_ ->
                                Decode.decode Decode.string input
                                    |> Expect.equal (Err Decode.ValidationError)
                    )
    in
    describe "Decode.decode Decode.string on bad input" <|
        (test "Cannot decode invalid base64 sequence" <|
            \_ ->
                Decode.decode Decode.string "/Ng9"
                    |> Expect.equal (Err Decode.InvalidByteSequence)
        )
            :: validationErrorTests



-- TODO: rename


identitySuite : Test
identitySuite =
    describe "Base64.Encoding and Base64.Decoding must be invertible"
        [ test "Encoding -> Decoding -> Idn" <|
            \_ ->
                let
                    input =
                        "String with utf8 characters 💩🚀"

                    encoded =
                        Encode.encode (Encode.string input)

                    decoded =
                        Decode.decode Decode.string encoded
                in
                Expect.equal (Ok input) decoded
        ]



-- Helpers


encodeHelper : List ( String, String ) -> (String -> String) -> List Test
encodeHelper testData coder =
    testData
        |> List.map
            (\( input, output ) ->
                test ("Can encode '" ++ input ++ "'") <|
                    \_ ->
                        coder input
                            |> Expect.equal output
            )


decodeHelper : List ( String, String ) -> (String -> Result Decode.Error String) -> List Test
decodeHelper testData coder =
    testData
        |> List.map
            (\( output, input ) ->
                test ("Can decode '" ++ input ++ "'") <|
                    \_ ->
                        coder input
                            |> Expect.equal (Ok output)
            )
