module Nem exposing (getAccount, Msg(..), nodeAddress)

{-| This library uses Http to speak to a given Nem Node


#

@docs Msg, nodeAddress, getAccount

-}

import Decoders
import Model exposing (Address, AccountMetaDataPair)
import Api


{-| -}
type Msg
    = AccountInfo AccountMetaDataPair
    | Noop


{-| nodeAddress returns a configuration for communicating with a NEM NIS node.

    defaultTestnet =
        nodeAddress "bigalice2.nem.ninja" 7809 "http"

-}
nodeAddress : String -> Int -> String -> Api.Config
nodeAddress host port_ protocol =
    Api.Config host protocol port_


{-| -}
getAccount : Api.Config -> Address -> Cmd Msg
getAccount config address =
    let
        handleIncoming result =
            case result of
                Ok acct_info ->
                    AccountInfo acct_info

                Err a ->
                    Noop
    in
        Api.get handleIncoming (accountRequest config address)


accountRequest : Api.Config -> Address -> Api.Request AccountMetaDataPair
accountRequest config address =
    let
        params =
            [ ( "address", Model.stripAddress address ) ]
    in
        Api.request config "account/get" params Decoders.decodeAccountMetaDataPair
