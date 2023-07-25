module Festival.Main exposing (..)

import Browser
import Festival.Model
import Festival.Model.Update
import Festival.Model.View
import Festival.Msg
import Json.Decode


main : Program Json.Decode.Value Festival.Model.Model Festival.Msg.Msg
main =
    Browser.document
        { init = Festival.Model.Update.init
        , update = Festival.Model.Update.update
        , subscriptions = Festival.Model.Update.subscriptions
        , view = Festival.Model.View.view
        }
