module Api exposing (Config, defaultTestnet, get, url)

import Http
import Json
import Url.Builder as Url


type alias Config =
    { host : String
    , protocol : String
    , port_ : Int
    }



--
-- type Action =
--   GetAccount Account


defaultTestnet : Config
defaultTestnet =
    { protocol = "http"
    , host = "bigalice2.nem.ninja"
    , port_ = 7890
    }



-- encodeQuery : List KeyValue -> String
-- encodeQuery list =
--     list
--         |> List.map (\( a, b ) -> a ++ "=" ++ b)
--         |> String.join "&"


url : Config -> List String -> List Url.QueryParameter -> String
url config endpoint keys_values =
    Url.crossOrigin
        (config.protocol ++ "://" ++ config.host ++ ":" ++ String.fromInt config.port_)
        endpoint
        keys_values


get : (Result Http.Error String -> msg) -> Config -> List String -> List Url.QueryParameter -> Json.Decoder String -> Cmd msg
get toMsg config endpoint keys_values decoder =
    Http.get
        { url = url config endpoint keys_values
        , expect = Http.expectJson toMsg decoder
        }
