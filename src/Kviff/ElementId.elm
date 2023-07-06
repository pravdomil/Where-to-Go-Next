module Kviff.ElementId exposing (..)

import Id
import Kviff.Data


type ElementId
    = Event (Id.Id Kviff.Data.Event)


toString : ElementId -> String
toString a =
    case a of
        Event b ->
            "event-" ++ Id.toString b
