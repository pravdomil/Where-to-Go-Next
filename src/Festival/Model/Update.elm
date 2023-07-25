module Festival.Model.Update exposing (..)

import Browser.Dom
import Festival.Data.Update
import Festival.ElementId
import Festival.Locale
import Festival.Model
import Festival.Model.Utils
import Festival.Msg
import Json.Decode
import Platform.Extra
import Task
import Time


init : Json.Decode.Value -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
init _ =
    ( Festival.Model.Model
        Festival.Locale.Czech
        (Time.millisToPosix 0)
        (Err Festival.Model.Loading)
    , Cmd.none
    )
        |> Platform.Extra.andThen getTime
        |> Platform.Extra.andThen getData


getTime : Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
getTime model =
    ( model
    , Time.now
        |> Task.perform Festival.Msg.TimeReceived
    )


getData : Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
getData model =
    ( model
    , Festival.Data.Update.get
        |> Task.attempt Festival.Msg.DataReceived
    )



--


update : Festival.Msg.Msg -> Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
update msg =
    case msg of
        Festival.Msg.LocaleRequested b ->
            \x -> ( { x | locale = b }, Cmd.none )

        Festival.Msg.TimeReceived b ->
            \x -> ( { x | time = b }, Cmd.none )

        Festival.Msg.DataReceived b ->
            case b of
                Ok c ->
                    \x ->
                        ( { x | data = Ok c }, Cmd.none )
                            |> Platform.Extra.andThen scrollToUpcomingEvent

                Err c ->
                    \x -> ( { x | data = Err (Festival.Model.HttpError c) }, Cmd.none )

        Festival.Msg.ViewportSet _ ->
            Platform.Extra.noOperation



--


subscriptions : Festival.Model.Model -> Sub Festival.Msg.Msg
subscriptions _ =
    Sub.none



--


scrollToUpcomingEvent : Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
scrollToUpcomingEvent model =
    case Festival.Model.Utils.relevantEvents model of
        ( id, _ ) :: _ ->
            ( model
            , Browser.Dom.getElement (Festival.ElementId.toString (Festival.ElementId.Event id))
                |> Task.andThen (\x -> Browser.Dom.setViewport x.element.x (x.element.y - 12))
                |> Task.attempt Festival.Msg.ViewportSet
            )

        _ ->
            Platform.Extra.noOperation model
