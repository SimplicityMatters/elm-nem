module ApiTests exposing (suite)

import Api
import Expect
import Model
import Test exposing (..)


defaultTestConfig : Api.Config
defaultTestConfig =
    Api.Config "127.0.0.1" "http" 7890


defaultTestAddress : Model.Address
defaultTestAddress =
    "TALICELCD3XPH4FFI5STGGNSNSWPOTG5E4DS2TOS"


suite : Test
suite =
    describe "Api.url"
        [ test "builds a url to get chain height" <|
            \_ ->
                let
                    params =
                        [ ( "address", defaultTestAddress ) ]
                in
                Api.url defaultTestConfig [ "chain", "height" ] []
                    |> Expect.equal "http://127.0.0.1:7890/chain/height"
        ]
