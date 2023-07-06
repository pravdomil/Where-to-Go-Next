module Kviff.Main exposing (..)

import Browser
import Json.Decode
import Kviff.Model
import Kviff.Model.Update
import Kviff.Model.View
import Kviff.Msg


main : Program Json.Decode.Value Kviff.Model.Model Kviff.Msg.Msg
main =
    Browser.document
        { init = Kviff.Model.Update.init
        , update = Kviff.Model.Update.update
        , subscriptions = Kviff.Model.Update.subscriptions
        , view = Kviff.Model.View.view
        }
