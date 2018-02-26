module Decoders exposing (..)

import Json.Decode exposing (int, float, string, list, maybe)
import Json.Decode.Pipeline exposing (decode, required)
import Model exposing (..)


type alias Decoder a =
    Json.Decode.Decoder a


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    decode AccountInfo
        |> required "address" string
        |> required "balance" int
        |> required "vestedBalance" int
        |> required "importance" float
        |> required "publicKey" (maybe string)
        |> required "harvestedBlocks" int


decodeAccountMetaDataPair : Decoder AccountMetaDataPair
decodeAccountMetaDataPair =
    decode AccountMetaDataPair
        |> required "account" decodeAccountInfo
        |> required "meta" decodeAccountMetaData


decodeAccountMetaData : Decoder AccountMetaData
decodeAccountMetaData =
    decode AccountMetaData
        |> required "status" string
        |> required "remoteStatus" string
        |> required "cosignatoryOf" (list decodeAccountInfo)
        |> required "cosignatories" (list decodeAccountInfo)
