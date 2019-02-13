module Nem.Address exposing (Address, DecodeError(..), fromBase32)

import Base32
import Bytes.Encode
import Keccak.Bytes as Keccak
import Result exposing (andThen)


type Address
    = Address String


type DecodeError
    = WrongFormat
    | InvalidChecksum
    | FaultyHash


fromBase32 : String -> Result DecodeError Address
fromBase32 string =
    string
        |> checkFormat
        |> andThen validChecksum
        |> andThen base32ify
        |> Result.map Address


stripAddress : String -> String
stripAddress string =
    String.replace "-" "" string


checkFormat : String -> Result DecodeError ( String, String )
checkFormat string =
    case String.length (stripAddress string) of
        25 ->
            Ok (partition string)

        _ ->
            Err WrongFormat


partition : String -> ( String, String )
partition string =
    ( String.dropRight 4 string, String.dropLeft 21 string )


validChecksum : ( String, String ) -> Result DecodeError (List Int)
validChecksum ( address_value, checksum ) =
    let
        given_checksum_bytes =
            checksum
                |> Base32.decode
                |> Result.withDefault []

        address_checksum_bytes =
            address_value
                |> Bytes.Encode.string
                |> Bytes.Encode.encode
                |> Keccak.fips202_sha3_256
    in
    case given_checksum_bytes == address_checksum_bytes of
        True ->
            Ok address_checksum_bytes

        False ->
            Err InvalidChecksum


base32ify : List Int -> Result DecodeError String
base32ify ints =
    case Base32.encode ints of
        Ok string ->
            Ok string

        Err error ->
            Err FaultyHash
