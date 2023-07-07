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
            List.filter
                (\( _, x ) ->
                    Time.posixToMillis (Kviff.Data.eventTime x) + Kviff.Data.eventDuration x > Time.posixToMillis model.time
                )
                (Dict.Any.toList data.events)
                |> List.sortBy (\( _, x ) -> Time.posixToMillis (Kviff.Data.eventTime x))

        Err _ ->
            []
