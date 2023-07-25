module Festival.Model exposing (..)

import Festival.Data
import Festival.Locale
import Http
import Time


type alias Model =
    { locale : Festival.Locale.Locale
    , time : Time.Posix
    , timeZone : Time.Zone
    , data : Result Error Festival.Data.Data
    }



--


type Error
    = Loading
    | HttpError Http.Error
