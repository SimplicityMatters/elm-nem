module Nem.Account exposing (Msg)

{-| This library uses Http to speak to a given Nem Node


#

@docs get

-}

import Api
import Json
import Model


type Msg
    = AccountInfo Model.AccountMetaDataPair
    | Noop



-- {-| -}
-- get : Api.Config -> Model.Address -> Cmd Msg
-- get config address =
--     let
--         handleIncoming result =
--             case result of
--                 Ok acct_info ->
--                     AccountInfo acct_info
--
--                 Err _ ->
--                     Noop
--     in
--     Api.get handleIncoming (getRequest config address)
--
--
-- getRequest : Api.Config -> Model.Address -> Api.Request Model.AccountMetaDataPair
-- getRequest config address =
--     let
--         params =
--             [ ( "address", Model.stripAddress address ) ]
--     in
--     Api.request config "account/get" params Json.decodeAccountMetaDataPair
