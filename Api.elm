module Api exposing (get, request, Config, Request, defaultTestnet, url)

import Decoders
import Http


type alias KeyValue =
    ( String, String )


type alias Config =
    { host : String
    , protocol : String
    , port_ : Int
    }


type alias Request a =
    Http.Request a


defaultTestnet : Config
defaultTestnet =
    Config "bigalice2.nem.ninja" "http" 7890


encodeQuery : List KeyValue -> String
encodeQuery list =
    list
        |> List.map (\( a, b ) -> a ++ "=" ++ b)
        |> String.join "&"


url : Config -> String -> List KeyValue -> String
url { protocol, host, port_ } endpoint keyvalue =
    protocol ++ "://" ++ host ++ ":" ++ (toString port_) ++ "/" ++ endpoint ++ "?" ++ encodeQuery keyvalue


get : (Result Http.Error decoder -> msg) -> Request decoder -> Cmd msg
get fn request =
    Http.send fn request


request : Config -> String -> List KeyValue -> Decoders.Decoder a -> Http.Request a
request config endpoint keyvalue decoder =
    Http.get (url config endpoint keyvalue) decoder
