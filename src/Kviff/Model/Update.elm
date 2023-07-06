module Kviff.Model.Update exposing (..)

import Browser.Dom
import Json.Decode
import Kviff.Api
import Kviff.Model
import Kviff.Msg
import Platform.Extra
import Task
import Time


init : Json.Decode.Value -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
init _ =
    ( Kviff.Model.Model
        Kviff.Api.Czech
        Nothing
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
    , Kviff.Api.getData
        |> Task.attempt Kviff.Msg.DataReceived
    )



--


update : Kviff.Msg.Msg -> Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
update msg =
    case msg of
        Kviff.Msg.LocaleChangeRequested b ->
            \x -> ( { x | locale = b }, Cmd.none )

        Kviff.Msg.TimeReceived b ->
            \x -> ( { x | time = Just b }, Cmd.none )

        Kviff.Msg.DataReceived b ->
            case b of
                Ok c ->
                    \x ->
                        ( { x | data = Ok (normalizeData c) }, Cmd.none )
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
        upcomingEvents : Result Kviff.Model.Error (List ( Int, Kviff.Api.Event ))
        upcomingEvents =
            model.data
                |> Result.map
                    (\v1 ->
                        v1
                            |> List.indexedMap Tuple.pair
                            |> List.filter
                                (\( _, v2 ) ->
                                    Maybe.map2
                                        (\startTime now ->
                                            Time.posixToMillis startTime > Time.posixToMillis now
                                        )
                                        v2.startTime
                                        model.time
                                        |> Maybe.withDefault False
                                )
                    )
    in
    case upcomingEvents |> Result.toMaybe |> Maybe.andThen List.head of
        Just ( id, _ ) ->
            ( model
            , Browser.Dom.getElement (eventId id)
                |> Task.andThen
                    (\v ->
                        Browser.Dom.setViewport v.element.x (v.element.y - 12)
                    )
                |> Task.attempt Kviff.Msg.ViewportSet
            )

        Nothing ->
            Platform.Extra.noOperation model



--


eventId : Int -> String
eventId a =
    "e-" ++ String.fromInt a


normalizeData : Kviff.Api.Data -> List Kviff.Api.Event
normalizeData a =
    Kviff.Api.dataToEvents a
        |> List.filter (\x -> x.type_ /= Kviff.Api.Restaurant)
        |> List.sortBy (\x -> Maybe.withDefault 0 (Maybe.map Time.posixToMillis x.startTime))
