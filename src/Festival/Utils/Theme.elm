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


fore70 =
    rgb 0.86 0.88 0.89


fore60 =
    rgb 0.8 0.82 0.84


fore50 =
    rgb 0.67 0.7 0.73


primary =
    rgb 0.2471 0.5961 1
