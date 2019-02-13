module RIPEMDTests exposing (suite)

import Expect
import RIPEMD
import Test exposing (..)


messageToHash message correct_hash _ =
    Expect.equal (RIPEMD.hash160 message) correct_hash


suite : Test
suite =
    describe "RIPEMD160"
        [ test "correctly hashes the empty string" <|
            messageToHash "" "9C1185A5C5E9FC54612808977EE8F548B2258D31"
        , test "correctly hashes 'a'" <|
            messageToHash "a" "0BDC9D2D256B3EE9DAAE347BE6F4DC835A467FFE"
        , test "correctly hashes 'abc'" <|
            messageToHash "abc" "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC"
        , test "correctly hashes 'message digest'" <|
            messageToHash "message digest" "5D0689EF49D2FAE572B881B123A85FFA21595F36"
        , test "correctly hashes 'A..Za..z0..9'" <|
            messageToHash "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" "B0E20B6E3116640286ED3A87A5713079B21F5189"

        -- very very slow; stopped it at 10min of computation time
        -- , skip <|
        --     test "correctly hashes 1 million 'a's" <|
        --         messageToHash (String.repeat 1000000 "a") "52783243C1697BDBE16D37F97F68F08325DC1528"
        ]
