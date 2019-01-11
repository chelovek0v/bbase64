module Base64.Shift exposing (Shift(..), and, decodeNext, next, shiftLeftBy, shiftRightZfBy, toInt)

import Bitwise


type Shift
    = Shift0
    | Shift2
    | Shift4
    | Shift6


toInt : Shift -> Int
toInt shift =
    case shift of
        Shift0 ->
            0

        Shift2 ->
            2

        Shift4 ->
            4

        Shift6 ->
            6


fromInt : Int -> Shift
fromInt numb =
    case numb of
        0 ->
            Shift0

        2 ->
            Shift2

        4 ->
            Shift4

        6 ->
            Shift6

        _ ->
            Shift0


next : Shift -> Shift
next shift =
    case shift of
        Shift0 ->
            Shift2

        Shift2 ->
            Shift4

        Shift4 ->
            Shift6

        Shift6 ->
            Shift2


decodeNext : Shift -> Shift
decodeNext shift =
    case shift of
        Shift0 ->
            Shift2

        Shift2 ->
            Shift4

        Shift4 ->
            Shift6

        Shift6 ->
            Shift0



-- Bitwise operations adjusted to Shift type


and : Shift -> Int -> Int
and shift value =
    case shift of
        Shift0 ->
            value

        Shift2 ->
            Bitwise.and 0x03 value

        Shift4 ->
            Bitwise.and 0x0F value

        Shift6 ->
            Bitwise.and 0x3F value


shiftLeftBy : Shift -> Int -> Int
shiftLeftBy shift value =
    Bitwise.shiftLeftBy (toInt shift) value


shiftRightZfBy : Shift -> Int -> Int
shiftRightZfBy shift value =
    Bitwise.shiftRightZfBy (toInt shift) value
