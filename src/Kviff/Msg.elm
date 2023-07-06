module Kviff.Msg exposing (..)

import Browser.Dom
import Http
import Kviff.Api
import Time


type Msg
    = ChangeLocale Kviff.Api.Locale
    | GotTime Time.Posix
    | GotData (Result Http.Error Kviff.Api.Data)
    | ViewportSet (Result Browser.Dom.Error ())
