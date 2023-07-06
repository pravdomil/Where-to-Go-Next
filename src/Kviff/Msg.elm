module Kviff.Msg exposing (..)

import Browser.Dom
import Http
import Kviff.Api
import Time


type Msg
    = LocaleRequested Kviff.Api.Locale
    | TimeReceived Time.Posix
      --
    | DataReceived (Result Http.Error Kviff.Api.Data)
      --
    | ViewportSet (Result Browser.Dom.Error ())
