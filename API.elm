module API exposing (..)

import Json.Decode exposing (Decoder, int, float, string)
import Json.Decode.Pipeline exposing (decode, required)

import API.Model exposing (..)

decoder : Decoder AccountInfo
decoder =
  decode AccountInfo
  |> required "address" string
  |> required "balance" int
  |> required "vestedBalance" int
  |> required "importance" float
  |> required "publicKey" string
  |> required "harvestedBlocks" int
