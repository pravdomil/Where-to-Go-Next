module Festival.Model.Update exposing (..)

import Browser.Dom
import Festival.ElementId
import Festival.Locale
import Festival.Model
import Festival.Model.Utils
import Festival.Msg
import Festival.Utils.Time
import Json.Decode
import Kviff
import Lfs
import Platform.Extra
import Task
import Time


init : Json.Decode.Value -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
init _ =
    ( Festival.Model.Model
        Festival.Locale.Czech
        (Time.millisToPosix 0)
        (Time.customZone 0 [])
        (Err Festival.Model.Loading)
    , Cmd.none
    )
        |> Platform.Extra.andThen getTimeAndZone


getTimeAndZone : Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
getTimeAndZone model =
    ( model
    , Task.perform
        Festival.Msg.TimeAndZoneReceived
        (Task.map2
            Tuple.pair
            Time.now
            Time.here
        )
    )


getData : Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
getData model =
    if
        (Festival.Utils.Time.monthToInt (Time.toMonth model.timeZone model.time) >= 7)
            && (Time.toDay model.timeZone model.time >= 15)
            |> always False
    then
        ( model, Lfs.get |> Task.attempt Festival.Msg.DataReceived )

    else
        ( model, Kviff.get |> Task.attempt Festival.Msg.DataReceived )



--


update : Festival.Msg.Msg -> Festival.Model.Model -> ( Festival.Model.Model, Cmd Festival.Msg.Msg )
update msg =
    case msg of
        Festival.Msg.LocaleRequested b ->
            \x -> ( { x | locale = b }, Cmd.none )

        Festival.Msg.TimeAndZoneReceived ( b, c ) ->
            (\x -> ( { x | time = b, timeZone = c }, Cmd.none ))
                >> Platform.Extra.andThen getData

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
