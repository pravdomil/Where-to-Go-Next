module Kviff.Model exposing (..)

import Http
import Kviff.Data
import Kviff.Locale
import Time


type alias Model =
    { locale : Kviff.Locale.Locale
    , time : Maybe Time.Posix
    , data : Result Error Kviff.Data.Data
    }



--


type Error
    = Loading
    | HttpError Http.Error
