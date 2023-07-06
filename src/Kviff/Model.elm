module Kviff.Model exposing (..)

import Http
import Kviff.Api
import Time


type alias Model =
    { locale : Kviff.Api.Locale
    , time : Maybe Time.Posix
    , data : Result Error (List Kviff.Api.Event)
    }



--


type Error
    = Loading
    | HttpError Http.Error
