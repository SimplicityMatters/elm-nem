module Main exposing (..)

import Http
import API.Model exposing (AccountMetaDataPair)

type Msg
  = AccountStatus (Result Http.Error AccountMetaDataPair)
