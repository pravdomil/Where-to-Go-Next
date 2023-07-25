module Festival.Msg exposing (..)

import Browser.Dom
import Festival.Data
import Festival.Locale
import Http
import Time


type Msg
    = LocaleRequested Festival.Locale.Locale
    | TimeAndZoneReceived ( Time.Posix, Time.Zone )
      --
    | DataReceived (Result Http.Error Festival.Data.Data)
      --
    | ViewportSet (Result Browser.Dom.Error ())
