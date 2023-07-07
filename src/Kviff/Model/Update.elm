module Kviff.Model.Update exposing (..)

import Browser.Dom
import Id
import Json.Decode
import Kviff.Data
import Kviff.Data.Update
import Kviff.ElementId
import Kviff.Locale
import Kviff.Model
import Kviff.Msg
import Platform.Extra
import Task
import Time


init : Json.Decode.Value -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
init _ =
    ( Kviff.Model.Model
        Kviff.Locale.Czech
        (Time.millisToPosix 0)
        (Err Kviff.Model.Loading)
    , Cmd.none
    )
        |> Platform.Extra.andThen getTime
        |> Platform.Extra.andThen getData


getTime : Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
getTime model =
    ( model
    , Time.now
        |> Task.perform Kviff.Msg.TimeReceived
    )


getData : Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
getData model =
    ( model
    , Kviff.Data.Update.get
        |> Task.attempt Kviff.Msg.DataReceived
    )



--


update : Kviff.Msg.Msg -> Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
update msg =
    case msg of
        Kviff.Msg.LocaleRequested b ->
            \x -> ( { x | locale = b }, Cmd.none )

        Kviff.Msg.TimeReceived b ->
            \x -> ( { x | time = b }, Cmd.none )

        Kviff.Msg.DataReceived b ->
            case b of
                Ok c ->
                    \x ->
                        ( { x | data = Ok c }, Cmd.none )
                            |> Platform.Extra.andThen scrollToUpcomingEvent

                Err c ->
                    \x -> ( { x | data = Err (Kviff.Model.HttpError c) }, Cmd.none )

        Kviff.Msg.ViewportSet _ ->
            Platform.Extra.noOperation



--


subscriptions : Kviff.Model.Model -> Sub Kviff.Msg.Msg
subscriptions _ =
    Sub.none



--


scrollToUpcomingEvent : Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
scrollToUpcomingEvent model =
    let
        upcomingEvents : Result Kviff.Model.Error (List ( Id.Id Kviff.Data.Event, Kviff.Data.Event ))
        upcomingEvents =
            model.data
                |> Result.map
                    (\x ->
                        x
                            |> List.indexedMap Tuple.pair
                            |> List.filter
                                (\( _, x2 ) ->
                                    Maybe.map2
                                        (\startTime now ->
                                            Time.posixToMillis startTime > Time.posixToMillis now
                                        )
                                        x2.startTime
                                        model.time
                                        |> Maybe.withDefault False
                                )
                    )
    in
    case upcomingEvents of
        Ok (( id, _ ) :: _) ->
            ( model
            , Browser.Dom.getElement (Kviff.ElementId.toString (Kviff.ElementId.Event id))
                |> Task.andThen (\x -> Browser.Dom.setViewport x.element.x (x.element.y - 12))
                |> Task.attempt Kviff.Msg.ViewportSet
            )

        _ ->
            Platform.Extra.noOperation model
