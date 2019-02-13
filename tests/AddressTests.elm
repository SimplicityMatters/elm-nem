module AddressTests exposing (suite)

import Expect
import Nem.Address as Address
import Test exposing (..)


suite : Test
suite =
    describe "Address"
        [ test "creates the same address, from pretty or not" <|
            \_ ->
                let
                    testAddressPretty =
                        "SCZJSB-7ZLHP4-XY45LH-VLIOFF-WDNSAM-IMRK2O-FVXW"
                            |> Address.fromBase32

                    testAddress =
                        "SCZJSB7ZLHP4XY45LHVLIOFFWDNSAMIMRK2OFVXW"
                            |> Address.fromBase32
                in
                Expect.equal testAddressPretty testAddress
        ]
