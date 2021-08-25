module Kviff.Api exposing (..)

import Http
import Http.Resolver as Resolver
import Iso8601
import Json.Decode as D
import Task exposing (Task)
import Time
import Utils.Json.Decode_ as D_


type alias Data =
    { films : List Film
    , events : List Event
    }


type alias Film =
    { id : Int

    --
    , name : String
    , nameLocalized : Localized String
    , author : String

    --
    , annotation : Localized String
    , description : Localized String
    , additionalDesc : Localized String
    , internalNotes : String

    --
    , year : Int
    , duration : Int
    , country : Localized String
    }


type alias Event =
    { id : Int
    , type_ : EventType

    --
    , name : Localized String
    , description : Localized String

    --
    , timeStart : Maybe Time.Posix
    , timeEnd : Maybe Time.Posix

    --
    , place : Place

    --
    , order : Int
    }


type EventType
    = Event_
    | Daily
    | Talk
    | Exhibition
    | Restaurant


type alias Place =
    { id : Int
    , name : Localized String

    --
    , address : String
    , gps : String

    --
    , phone : String
    , website : String
    }



--


type Locale
    = English
    | Czech


type alias Localized a =
    { en : a
    , cz : a
    }


localize : Locale -> Localized a -> a
localize locale a =
    case locale of
        English ->
            a.en

        Czech ->
            a.cz



--


getEvents : Task Http.Error (List Event)
getEvents =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://www.kviff.com/en/exports/json/acmp-events"
        , body = Http.emptyBody
        , resolver = Resolver.json decodeEvents
        , timeout = Just 30000
        }



--


decodeFilm : D.Decoder Film
decodeFilm =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 ->
            { id = v1
            , nameLocalized = Localized v2 v3
            , name = v4
            , author = v5
            , annotation = Localized v6 v7
            , description = Localized v8 v9
            , additionalDesc = Localized v10 v11
            , internalNotes = v12
            , year = v13
            , duration = v14
            , country = Localized v15 v16
            }
        )
        (D.field "id_film" D.int)
        (D.field "nazev_en" D.string)
        (D.field "nazev_cz" D.string)
        (D.field "nazev_orig" D.string)
        (D.field "author" D.string)
        (D.field "anotace_en" D.string)
        (D.field "anotace_cz" D.string)
        (D.field "film_en" D.string)
        |> D_.apply (D.field "film_cz" D.string)
        |> D_.apply (D.field "doplnujici_text_en" D.string)
        |> D_.apply (D.field "doplnujici_text_cz" D.string)
        |> D_.apply (D.field "poznamky" D.string)
        |> D_.apply (D.field "rok" D.int)
        |> D_.apply (D.field "delka" D.int)
        |> D_.apply (D.field "zeme_en" D.string)
        |> D_.apply (D.field "zeme_cz" D.string)


decodeEvents : D.Decoder (List Event)
decodeEvents =
    D.field "typ" (D.list (D.field "den" (D.list (D.field "akce" (D.list decodeEvent)))))
        |> D.map (List.concat >> List.concat)


decodeEvent : D.Decoder Event
decodeEvent =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 ->
            { id = v3
            , type_ = v16

            --
            , name = Localized v12 v11
            , description = Localized v15 v14

            --
            , timeStart = v2
            , timeEnd = v1

            --
            , place =
                { id = v8
                , name = Localized v6 v5

                --
                , address = v4
                , gps = v7

                --
                , phone = v9
                , website = v10
                }

            --
            , order = v13
            }
        )
        (D.field "cas_do" (D_.maybe Iso8601.decoder))
        (D.field "cas_od" (D_.maybe Iso8601.decoder))
        (D.field "id" D.int)
        (D.field "misto_adresa" D.string)
        (D.field "misto_cz" D.string)
        (D.field "misto_en" D.string)
        (D.field "misto_gps" D.string)
        (D.field "misto_id" D.int)
        |> D_.apply (D.field "misto_telefon" D.string)
        |> D_.apply (D.field "misto_website" D.string)
        |> D_.apply (D.field "nazev_cz" D.string)
        |> D_.apply (D.field "nazev_en" D.string)
        |> D_.apply (D.field "order" D.int)
        |> D_.apply (D.field "popis_cz" D.string)
        |> D_.apply (D.field "popis_en" D.string)
        |> D_.apply (D.field "typ" decodeEventType)


decodeEventType : D.Decoder EventType
decodeEventType =
    D.string
        |> D.andThen
            (\v ->
                case v of
                    "Akce" ->
                        D.succeed Event_

                    "Denně" ->
                        D.succeed Daily

                    "KVIFF Talk" ->
                        D.succeed Talk

                    "Výstava" ->
                        D.succeed Exhibition

                    "Restaurace" ->
                        D.succeed Restaurant

                    _ ->
                        D.fail ("Unknow type " ++ v ++ ".")
            )
