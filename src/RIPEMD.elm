module RIPEMD exposing (bytesToWord, hash160, rotl, wordToBytes)

-- import Bytes.Encode
-- import Bytes.Decode

import Array exposing (Array)
import Base16
import Bitwise exposing (..)
import String.UTF8


initialState =
    { a = 0x67452301
    , b = 0xEFCDAB89
    , c = 0x98BADCFE
    , d = 0x10325476
    , e_ = 0xC3D2E1F0
    }


{-| hash160 takes a URF8 string and returns its RIPEMD160 hash in hex
-}
hash160 : String -> String
hash160 message =
    let
        msg_bytes =
            String.UTF8.toBytes message

        len =
            List.length msg_bytes

        padding =
            64 - remainderBy 64 (len + 1)

        padding_with_len =
            if padding < 8 then
                padding + 64

            else
                padding

        -- FIXME we have a length limit at 2^53 - 1 -- show this
        len_low =
            wordToBytes <| and (len * 8) 0xFFFFFFFF

        len_hi =
            wordToBytes <| ((len * 8) // 0x0000000100000000)

        bytes =
            msg_bytes
                ++ [ 0x80 ]
                ++ List.repeat (padding_with_len - 8) 0x00
                ++ len_low
                ++ len_hi

        -- FIXME what about 'update' syntax
        words =
            List.foldl
                (\x ( bytes_, words_ ) ->
                    let
                        x_ =
                            bytes_ ++ [ x ]
                    in
                    case x_ of
                        [ a, b, c, d ] ->
                            ( [], words_ ++ [ bytesToWord a b c d ] )

                        _ ->
                            ( x_, words_ )
                )
                ( [], [] )
                bytes
                |> Tuple.second

        fs =
            List.foldl
                (\x ( w_, s_ ) ->
                    let
                        words_ =
                            w_ ++ [ x ]
                    in
                    case List.length words_ of
                        16 ->
                            ( [], compute words_ s_ )

                        _ ->
                            ( words_, s_ )
                )
                ( [], initialState )
                words
                |> Tuple.second
    in
    Result.withDefault "" <|
        Base16.encode <|
            List.concat <|
                List.map wordToBytes [ fs.a, fs.b, fs.c, fs.d, fs.e_ ]


bytesToWord a b c d =
    let
        word =
            a + shiftLeftBy 8 b + shiftLeftBy 16 c + shiftLeftBy 24 d
    in
    word


wordToBytes w =
    [ and 0xFF w, and 0xFF00 w |> shiftRightBy 8, and 0x00FF0000 w |> shiftRightBy 16, and 0xFF000000 w |> shiftRightBy 24 ]
        |> List.map (and 0xFF)


compute : List Int -> State -> State
compute words state =
    let
        word_array =
            Array.fromList words

        ( l, r ) =
            List.foldl (oneComputeStep word_array) ( state, state ) (List.range 0 79)
    in
    { a = and 0xFFFFFFFF <| state.b + l.c + r.d
    , b = and 0xFFFFFFFF <| state.c + l.d + r.e_
    , c = and 0xFFFFFFFF <| state.d + l.e_ + r.a
    , d = and 0xFFFFFFFF <| state.e_ + l.a + r.b
    , e_ = and 0xFFFFFFFF <| state.a + l.b + r.c
    }


type alias State =
    { a : Int, b : Int, c : Int, d : Int, e_ : Int }


type alias RoundSpecifics =
    { fn : Int -> Int -> Int -> Int, z : Int, h : Int, s : Int }


getRoundSpecifics : Int -> ( RoundSpecifics, RoundSpecifics )
getRoundSpecifics i =
    let
        j =
            i // 16

        ( fn_l, fn_r ) =
            case j of
                0 ->
                    ( fn1, fn5 )

                1 ->
                    ( fn2, fn4 )

                2 ->
                    ( fn3, fn3 )

                3 ->
                    ( fn4, fn2 )

                _ ->
                    ( fn5, fn1 )
    in
    ( RoundSpecifics fn_l (get i zl) (get j hl) (get i sl)
    , RoundSpecifics fn_r (get i zr) (get j hr) (get i sr)
    )


oneComputeStep : Array Int -> Int -> ( State, State ) -> ( State, State )
oneComputeStep words i ( l, r ) =
    let
        ( rs_l, rs_r ) =
            getRoundSpecifics i

        wl =
            get rs_l.z words

        wr =
            get rs_r.z words

        tl =
            and 0xFFFFFFFF <| f rs_l.fn l.a l.b l.c l.d l.e_ wl rs_l.h rs_l.s

        tr =
            and 0xFFFFFFFF <| f rs_r.fn r.a r.b r.c r.d r.e_ wr rs_r.h rs_r.s
    in
    ( State l.e_ tl l.b (rotl l.c 10) l.d, State r.e_ tr r.b (rotl r.c 10) r.d )



-- DETAILS


rotl x n =
    or (shiftLeftBy n x) (shiftRightZfBy (32 - n) x)


safe_add x y =
    and 0xFFFFFFFF (x + y)


f fn a_ b_ c_ d_ e_ m_ k_ s_ =
    safe_add (rotl (safe_add (safe_add a_ (fn b_ c_ d_)) (safe_add m_ k_)) s_) e_


fn1 x y z =
    Bitwise.xor (Bitwise.xor x y) z


fn2 x y z =
    or (and x y) (and (complement x) z)


fn3 x y z =
    Bitwise.xor (or x (complement y)) z


fn4 x y z =
    or (and x z) (and y (complement z))


fn5 x y z =
    Bitwise.xor x (or y (complement z))


get idx array =
    Maybe.withDefault 0 <| Array.get idx array


hl =
    [ 0x00, 0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xA953FD4E ] |> Array.fromList


hr =
    [ 0x50A28BE6, 0x5C4DD124, 0x6D703EF3, 0x7A6D76E9, 0x00 ] |> Array.fromList


zl =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 10
    , 11
    , 12
    , 13
    , 14
    , 15
    , 7
    , 4
    , 13
    , 1
    , 10
    , 6
    , 15
    , 3
    , 12
    , 0
    , 9
    , 5
    , 2
    , 14
    , 11
    , 8
    , 3
    , 10
    , 14
    , 4
    , 9
    , 15
    , 8
    , 1
    , 2
    , 7
    , 0
    , 6
    , 13
    , 11
    , 5
    , 12
    , 1
    , 9
    , 11
    , 10
    , 0
    , 8
    , 12
    , 4
    , 13
    , 3
    , 7
    , 15
    , 14
    , 5
    , 6
    , 2
    , 4
    , 0
    , 5
    , 9
    , 7
    , 12
    , 2
    , 10
    , 14
    , 1
    , 3
    , 8
    , 11
    , 6
    , 15
    , 13
    ]
        |> Array.fromList


zr =
    [ 5
    , 14
    , 7
    , 0
    , 9
    , 2
    , 11
    , 4
    , 13
    , 6
    , 15
    , 8
    , 1
    , 10
    , 3
    , 12
    , 6
    , 11
    , 3
    , 7
    , 0
    , 13
    , 5
    , 10
    , 14
    , 15
    , 8
    , 12
    , 4
    , 9
    , 1
    , 2
    , 15
    , 5
    , 1
    , 3
    , 7
    , 14
    , 6
    , 9
    , 11
    , 8
    , 12
    , 2
    , 10
    , 0
    , 4
    , 13
    , 8
    , 6
    , 4
    , 1
    , 3
    , 11
    , 15
    , 0
    , 5
    , 12
    , 2
    , 13
    , 9
    , 7
    , 10
    , 14
    , 12
    , 15
    , 10
    , 4
    , 1
    , 5
    , 8
    , 7
    , 6
    , 2
    , 13
    , 14
    , 0
    , 3
    , 9
    , 11
    ]
        |> Array.fromList


sl =
    [ 11
    , 14
    , 15
    , 12
    , 5
    , 8
    , 7
    , 9
    , 11
    , 13
    , 14
    , 15
    , 6
    , 7
    , 9
    , 8
    , 7
    , 6
    , 8
    , 13
    , 11
    , 9
    , 7
    , 15
    , 7
    , 12
    , 15
    , 9
    , 11
    , 7
    , 13
    , 12
    , 11
    , 13
    , 6
    , 7
    , 14
    , 9
    , 13
    , 15
    , 14
    , 8
    , 13
    , 6
    , 5
    , 12
    , 7
    , 5
    , 11
    , 12
    , 14
    , 15
    , 14
    , 15
    , 9
    , 8
    , 9
    , 14
    , 5
    , 6
    , 8
    , 6
    , 5
    , 12
    , 9
    , 15
    , 5
    , 11
    , 6
    , 8
    , 13
    , 12
    , 5
    , 12
    , 13
    , 14
    , 11
    , 8
    , 5
    , 6
    ]
        |> Array.fromList


sr =
    [ 8
    , 9
    , 9
    , 11
    , 13
    , 15
    , 15
    , 5
    , 7
    , 7
    , 8
    , 11
    , 14
    , 14
    , 12
    , 6
    , 9
    , 13
    , 15
    , 7
    , 12
    , 8
    , 9
    , 11
    , 7
    , 7
    , 12
    , 7
    , 6
    , 15
    , 13
    , 11
    , 9
    , 7
    , 15
    , 11
    , 8
    , 6
    , 6
    , 14
    , 12
    , 13
    , 5
    , 14
    , 13
    , 13
    , 7
    , 5
    , 15
    , 5
    , 8
    , 11
    , 14
    , 14
    , 6
    , 14
    , 6
    , 9
    , 12
    , 9
    , 12
    , 5
    , 15
    , 8
    , 8
    , 5
    , 12
    , 9
    , 12
    , 5
    , 14
    , 6
    , 8
    , 13
    , 6
    , 5
    , 15
    , 13
    , 11
    , 11
    ]
        |> Array.fromList
