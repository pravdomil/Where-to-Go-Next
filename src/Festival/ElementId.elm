module Festival.ElementId exposing (..)

import Element.PravdomilUi
import Festival.Data
import Id


type ElementId
    = Event (Id.Id Festival.Data.Event)


toString : ElementId -> String
toString a =
    case a of
        Event b ->
            "event-" ++ Id.toString b


toId : ElementId -> Element.PravdomilUi.Attribute msg
toId a =
    Element.PravdomilUi.id (toString a)
