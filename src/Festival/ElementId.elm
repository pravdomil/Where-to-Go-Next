module Festival.ElementId exposing (..)

import Element
import Festival.Data
import Html.Attributes
import Id


type ElementId
    = Event (Id.Id Festival.Data.Event)


toString : ElementId -> String
toString a =
    case a of
        Event b ->
            "event-" ++ Id.toString b


toId : ElementId -> Element.Attribute msg
toId a =
    Element.htmlAttribute (Html.Attributes.id (toString a))
