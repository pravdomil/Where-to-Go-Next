module Kviff.Msg exposing (..)

import Browser.Dom
import Http
import Kviff.Data
import Time


type Msg
    = LocaleRequested Kviff.Data.Locale
    | TimeReceived Time.Posix
      --
    | DataReceived (Result Http.Error Kviff.Data.Data)
      --
    | ViewportSet (Result Browser.Dom.Error ())
