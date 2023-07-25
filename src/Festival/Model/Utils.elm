module Festival.Model.Utils exposing (..)

import Dict.Any
import Festival.Data
import Festival.Model
import Id
import Time


relevantEvents : Festival.Model.Model -> List ( Id.Id Festival.Data.Event, Festival.Data.Event )
relevantEvents model =
    case model.data of
        Ok data ->
            Dict.Any.toList data.events
                |> List.filter (eventIsRelevant model)
                |> sortEvents

        Err _ ->
            []


sortEvents : List ( Id.Id Festival.Data.Event, Festival.Data.Event ) -> List ( Id.Id Festival.Data.Event, Festival.Data.Event )
sortEvents a =
    List.sortBy
        (\( _, x ) ->
            ( Time.posixToMillis (Festival.Data.eventTime x)
            , Festival.Data.eventDuration x
            )
        )
        a


eventIsRelevant : Festival.Model.Model -> ( Id.Id Festival.Data.Event, Festival.Data.Event ) -> Bool
eventIsRelevant model ( _, a ) =
    Time.posixToMillis (Festival.Data.eventTime a) + Festival.Data.eventDuration a > Time.posixToMillis model.time
