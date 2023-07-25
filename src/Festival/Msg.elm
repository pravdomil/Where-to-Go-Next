module Festival.Msg exposing (..)

import Browser.Dom
import Festival.Data
import Festival.Locale
import Http
import Time


type Msg
    = LocaleRequested Festival.Locale.Locale
    | TimeReceived Time.Posix
      --
    | DataReceived (Result Http.Error Festival.Data.Data)
      --
    | ViewportSet (Result Browser.Dom.Error ())
