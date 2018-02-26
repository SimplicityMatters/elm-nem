module Nem exposing (getAccount, Msg(..), Config)

import Decoders
import Model exposing (Address, AccountMetaDataPair)
import Api


type alias Config =
    Api.Config


type Msg
    = AccountInfo AccountMetaDataPair
    | Noop


getAccount : Config -> Address -> Cmd Msg
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


stripAddress : String -> String
stripAddress =
    String.filter (\c -> c /= '-')


accountRequest : Config -> Address -> Api.Request AccountMetaDataPair
accountRequest config address =
    let
        params =
            [ ( "address", stripAddress address ) ]
    in
        Api.request config "account/get" params Decoders.decodeAccountMetaDataPair
