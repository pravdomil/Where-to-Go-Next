module Kviff.Model.Update exposing (..)

import Browser.Dom
import Json.Decode
import Kviff.Api
import Kviff.Model
import Kviff.Msg
import Task
import Time


init : Json.Decode.Value -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
init _ =
    ( Kviff.Model.Model
        Kviff.Api.Czech
        Nothing
        (Err Kviff.Model.Loading)
    , Cmd.batch
        [ Time.now
            |> Task.perform Kviff.Msg.GotTime
        , Kviff.Api.getData
            |> Task.attempt Kviff.Msg.GotData
        ]
    )



--


update : Kviff.Msg.Msg -> Kviff.Model.Model -> ( Kviff.Model.Model, Cmd Kviff.Msg.Msg )
update msg model =
    case msg of
        Kviff.Msg.ChangeLocale b ->
            ( { model | locale = b }
            , Cmd.none
            )

        Kviff.Msg.GotTime b ->
            ( { model | time = Just b }
            , Cmd.none
            )

        Kviff.Msg.GotData b ->
            let
                nextModel : Kviff.Model.Model
                nextModel =
                    { model
                        | data = b |> Result.map normalizeData |> Result.mapError Kviff.Model.HttpError
                    }
            in
            ( nextModel
            , scrollToUpcomingEvent nextModel |> Task.attempt Kviff.Msg.ViewportSet
            )

        Kviff.Msg.ViewportSet _ ->
            ( model
            , Cmd.none
            )



--


subscriptions : Kviff.Model.Model -> Sub Kviff.Msg.Msg
subscriptions _ =
    Sub.none



--


scrollToUpcomingEvent : Kviff.Model.Model -> Task.Task Browser.Dom.Error ()
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
            Browser.Dom.getElement (eventId id)
                |> Task.andThen
                    (\v ->
                        Browser.Dom.setViewport v.element.x (v.element.y - 12)
                    )

        Nothing ->
            Task.succeed ()



--


eventId : Int -> String
eventId a =
    "e-" ++ String.fromInt a


normalizeData : Kviff.Api.Data -> List Kviff.Api.Event
normalizeData a =
    a
        |> Kviff.Api.dataToEvents
        |> List.filter
            (\v ->
                v.type_ /= Kviff.Api.Restaurant
            )
        |> List.sortBy
            (\v ->
                v.startTime
                    |> Maybe.map Time.posixToMillis
                    |> Maybe.withDefault 0
            )
