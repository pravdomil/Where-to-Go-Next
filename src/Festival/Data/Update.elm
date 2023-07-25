module Festival.Data.Update exposing (..)

import Festival.Data
import Http
import Http.Resolver
import Json.Decode
import Task


get : Task.Task Http.Error Festival.Data.Data
get =
    let
        request : Json.Decode.Decoder a -> String -> Task.Task Http.Error a
        request decoder a =
            Http.task
                { method = "GET"
                , headers = []
                , url = "https://www.kviff.com/en/exports/json/" ++ a
                , body = Http.emptyBody
                , resolver = Http.Resolver.json decoder
                , timeout = Just 30000
                }
    in
    request Festival.Data.decoder "catalog"
