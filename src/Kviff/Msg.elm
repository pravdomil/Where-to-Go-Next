module Kviff.Msg exposing (..)

import Browser.Dom
import Http
import Kviff.Data
import Kviff.Locale
import Time


type Msg
    = LocaleRequested Kviff.Locale.Locale
    | TimeReceived Time.Posix
      --
    | DataReceived (Result Http.Error Kviff.Data.Data)
      --
    | ViewportSet (Result Browser.Dom.Error ())
