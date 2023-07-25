module Lfs exposing (..)

import Dict.Any
import Festival.Data
import Http
import Http.Resolver
import Id
import Regex
import Task
import Url


get : Task.Task Http.Error Festival.Data.Data
get =
    getIds
        |> Task.andThen (\x -> Task.sequence (List.map getData (List.take 1 x)))
        |> Task.map (List.foldl mergeData Festival.Data.empty)


getIds : Task.Task Http.Error (List (Id.Id Festival.Data.Film))
getIds =
    let
        regex : Regex.Regex
        regex =
            Regex.fromString "href=\"/detail/\\?film=([^\"]+)\"" |> Maybe.withDefault Regex.never

        decoder : String -> Result String (List (Id.Id Festival.Data.Film))
        decoder a =
            Regex.find regex a
                |> List.filterMap
                    (\x ->
                        List.head x.submatches
                            |> Maybe.andThen identity
                            |> Maybe.andThen Url.percentDecode
                            |> Maybe.map Id.fromString
                    )
                |> Ok
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://program.lfs.cz/"
        , body = Http.emptyBody
        , resolver = Http.stringResolver (Http.Resolver.helper decoder)
        , timeout = Just 30000
        }


getData : Id.Id Festival.Data.Film -> Task.Task Http.Error Festival.Data.Data
getData _ =
    Task.succeed Festival.Data.empty



--


mergeData : Festival.Data.Data -> Festival.Data.Data -> Festival.Data.Data
mergeData acc a =
    Festival.Data.Data
        (Dict.Any.union Id.toString a.events acc.events)
        (Dict.Any.union Id.toString a.films acc.films)
        (Dict.Any.union Id.toString a.categories acc.categories)
        (Dict.Any.union Id.toString a.places acc.places)
