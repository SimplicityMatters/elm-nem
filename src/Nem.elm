module Nem exposing (nodeAddress)

{-| This library uses Http to speak to a given Nem Node


#

@docs nodeAddress

-}

import Api
import Keccak


{-| nodeAddress returns a configuration for communicating with a NEM NIS node.

    defaultTestnet =
        nodeAddress "bigalice2.nem.ninja" 7809 "http"

-}
nodeAddress : String -> Int -> String -> Api.Config
nodeAddress host port_ protocol =
    Api.Config host protocol port_



-- address hash = Nem.Address ->
