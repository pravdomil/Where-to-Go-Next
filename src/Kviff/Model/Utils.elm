module Kviff.Model.Utils exposing (..)

import Dict.Any
import Id
import Kviff.Data
import Kviff.Model
import Time


relevantEvents : Kviff.Model.Model -> List ( Id.Id Kviff.Data.Event, Kviff.Data.Event )
relevantEvents model =
    case model.data of
        Ok data ->
            Dict.Any.toList data.events
                |> List.filter (eventIsRelevant model)
                |> sortEvents

        Err _ ->
            []


sortEvents : List ( Id.Id Kviff.Data.Event, Kviff.Data.Event ) -> List ( Id.Id Kviff.Data.Event, Kviff.Data.Event )
sortEvents a =
    List.sortBy
        (\( _, x ) ->
            ( Time.posixToMillis (Kviff.Data.eventTime x)
            , Kviff.Data.eventDuration x
            )
        )
        a


eventIsRelevant : Kviff.Model.Model -> ( Id.Id Kviff.Data.Event, Kviff.Data.Event ) -> Bool
eventIsRelevant model ( _, a ) =
    Time.posixToMillis (Kviff.Data.eventTime a) + Kviff.Data.eventDuration a > Time.posixToMillis model.time
