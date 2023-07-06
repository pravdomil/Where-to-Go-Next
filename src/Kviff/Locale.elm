module Kviff.Locale exposing (..)


type Locale
    = English
    | Czech



--


type alias Localized a =
    { en : a
    , cz : a
    }


localize : Locale -> Localized a -> a
localize locale a =
    case locale of
        English ->
            a.en

        Czech ->
            a.cz
