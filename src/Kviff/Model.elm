module Kviff.Model exposing (..)

import Http
import Kviff.Data
import Time


type alias Model =
    { locale : Kviff.Data.Locale
    , time : Maybe Time.Posix
    , data : Result Error Kviff.Data.Data
    }



--


type Error
    = Loading
    | HttpError Http.Error
