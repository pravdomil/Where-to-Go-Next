module Festival.Utils.Theme exposing (..)

import Element exposing (..)
import Element.Border
import Element.Font
import Element.Region
import Html
import Html.Attributes


type alias EdgesXY =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }



--


black =
    rgb 0 0 0


white =
    rgb 1 1 1


lightText =
    rgb 0.86 0.86 0.86


mutedText =
    rgb 0.68 0.68 0.68


blue =
    rgb 0.25 0.6 1
