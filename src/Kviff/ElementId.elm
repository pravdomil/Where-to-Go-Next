module Kviff.ElementId exposing (..)

import Id
import Kviff.Api


type ElementId
    = Event (Id.Id Kviff.Api.Event)


toString : ElementId -> String
toString a =
    case a of
        Event b ->
            "event-" ++ Id.toString b
