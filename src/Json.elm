module Json exposing (Decoder, decodeAccountInfo, decodeAccountMetaData, decodeAccountMetaDataPair)

import Json.Decode exposing (float, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Model exposing (..)


type alias Decoder a =
    Json.Decode.Decoder a


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    succeed AccountInfo
        |> required "address" string
        |> required "balance" int
        |> required "vestedBalance" int
        |> required "importance" float
        |> required "publicKey" (maybe string)
        |> required "harvestedBlocks" int


decodeAccountMetaDataPair : Decoder AccountMetaDataPair
decodeAccountMetaDataPair =
    succeed AccountMetaDataPair
        |> required "account" decodeAccountInfo
        |> required "meta" decodeAccountMetaData


decodeAccountMetaData : Decoder AccountMetaData
decodeAccountMetaData =
    succeed AccountMetaData
        |> required "status" string
        |> required "remoteStatus" string
        |> required "cosignatoryOf" (list decodeAccountInfo)
        |> required "cosignatories" (list decodeAccountInfo)
