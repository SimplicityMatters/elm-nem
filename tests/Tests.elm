module Tests exposing (..)

import Test exposing (..)
import Expect
import Model
import Api


defaultTestConfig : Api.Config
defaultTestConfig =
    Api.Config "127.0.0.1" "http" 7890


defaultTestAddress : Model.Address
defaultTestAddress =
    "TALICELCD3XPH4FFI5STGGNSNSWPOTG5E4DS2TOS"


suite : Test
suite =
    describe "Nem.Internal.url"
        [ test "confirm output" <|
            \_ ->
                let
                    params =
                        [ ( "address", defaultTestAddress ) ]
                in
                    Api.url defaultTestConfig "account/get" params
                        |> Expect.equal ("http://127.0.0.1:7890/account/get?address=" ++ defaultTestAddress)
        ]
