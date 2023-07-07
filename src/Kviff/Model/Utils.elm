module Kviff.Model.Utils exposing (..)

import Dict.Any
import Id
import Kviff.Data
import Kviff.Model
import Time


upcomingEvents : Kviff.Model.Model -> List ( Id.Id Kviff.Data.Event, Kviff.Data.Event )
upcomingEvents model =
    let
        hour : Int
        hour =
            60 * 60 * 1000
    in
    case model.data of
        Ok data ->
            List.filter
                (\( _, x ) ->
                    Time.posixToMillis (Kviff.Data.eventTime x) > (Time.posixToMillis model.time - hour)
                )
                (Dict.Any.toList data.events)
                |> List.sortBy (\( _, x ) -> Time.posixToMillis (Kviff.Data.eventTime x))

        Err _ ->
            []
