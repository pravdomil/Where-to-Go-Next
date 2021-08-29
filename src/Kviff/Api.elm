module Kviff.Api exposing (..)

import Gps as Gps exposing (Gps)
import Http
import Http.Resolver as Resolver
import Iso8601
import Json.Decode as D
import Task exposing (Task)
import Time
import Url exposing (Url)
import Utils.Json.Decode_ as D_


type alias Data =
    { films : List Film
    , events : List Event
    }



--


type alias Film =
    { id : Int

    --
    , name : String
    , nameLocalized : Localized String
    , author : String
    , images : List String

    --
    , annotation : Localized String
    , description : Localized String
    , additionalDesc : Localized String
    , internalNote : String

    --
    , year : Int
    , duration : Int
    , country : Localized String

    --
    , screenings : List Screening
    }


type alias Screening =
    { code : String
    , name : Localized String
    , timeStart : Time.Posix

    --
    , theatreId : Int
    , theatreCode : String
    , theatreName : Localized String

    --
    , theatreAddress : String
    , theatreGps : Gps
    }



--


type alias Event =
    { id : Maybe Int
    , filmId : Maybe Int
    , type_ : EventType

    --
    , name : Localized String
    , image : Maybe String
    , description : Localized String
    , note : Localized String

    --
    , timeStart : Maybe Time.Posix
    , timeEnd : Maybe Time.Posix

    --
    , place : Place
    }


type EventType
    = Screening_
    | Event_
    | Daily
    | Talk
    | Exhibition
    | Restaurant


type alias Place =
    { id : Int
    , name : Localized String

    --
    , address : String
    , gps : Maybe Gps

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


getData : Task Http.Error Data
getData =
    let
        request : D.Decoder a -> String -> Task Http.Error a
        request decoder a =
            Http.task
                { method = "GET"
                , headers = []
                , url = "https://www.kviff.com/en/exports/json/" ++ a
                , body = Http.emptyBody
                , resolver = Resolver.json decoder
                , timeout = Just 30000
                }
    in
    Task.map2 Data
        (request decodeFilms "catalog")
        (request decodeEvents "acmp-events")



--


decodeFilms : D.Decoder (List Film)
decodeFilms =
    D.field "sekce" (D.list (D.field "subsekce" (D.list (D.field "film" (D.list decodeFilm)))))
        |> D.map (List.concat >> List.concat)


decodeFilm : D.Decoder Film
decodeFilm =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 ->
            { id = v1
            , nameLocalized = Localized v2 v3
            , name = v4
            , author = v5
            , annotation = Localized v6 v7
            , description = Localized v8 v9
            , additionalDesc = Localized v10 v11
            , internalNote = v12
            , year = v13
            , duration = v14
            , country = Localized v15 v16
            , screenings = v17
            , images = v18
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
        |> D_.apply (D.field "screenings" (D.field "screening" (D.list decodeFilmScreening)))
        |> D_.apply (D.field "film_web_images" (D.field "image" (D.list decodeFilmImage)))


decodeFilmScreening : D.Decoder Screening
decodeFilmScreening =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 ->
            { code = v1
            , name = Localized v2 v3
            , timeStart = v4
            , theatreId = v5
            , theatreCode = v6
            , theatreName = Localized v7 v8
            , theatreAddress = v9
            , theatreGps = v10
            }
        )
        (D.field "code" D.string)
        (D.field "title_en" D.string)
        (D.field "title_cz" D.string)
        (D.field "timestamp" decodePosix)
        (D.field "theatre_misto_id" D.int)
        (D.field "theatre_code" D.string)
        (D.field "theatre_en" D.string)
        (D.field "theatre_cz" D.string)
        |> D_.apply (D.field "theatre_misto_adresa" D.string)
        |> D_.apply (D.field "theatre_misto_gps" Gps.decode)


decodeFilmImage : D.Decoder String
decodeFilmImage =
    D.map2
        (\v1 v2 ->
            "https://www.kviff.com/en/image/fancybox/" ++ Url.percentEncode (String.fromInt v1) ++ "/" ++ Url.percentEncode v2
        )
        (D.field "idWebImage" D.int)
        (D.field "validationCode" D.string)


decodeEvents : D.Decoder (List Event)
decodeEvents =
    D.field "typ" (D.list (D.field "den" (D.list (D.field "akce" (D.list decodeEvent)))))
        |> D.map (List.concat >> List.concat)


decodeEvent : D.Decoder Event
decodeEvent =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 ->
            { id = Just v3
            , filmId = Nothing
            , type_ = v15

            --
            , name = Localized v12 v11
            , image = Nothing
            , description = Localized v14 v13
            , note = Localized "" ""

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
            }
        )
        (D.field "cas_do" (D_.maybe decodePosix))
        (D.field "cas_od" (D_.maybe decodePosix))
        (D.field "id" D.int)
        (D.field "misto_adresa" D.string)
        (D.field "misto_cz" D.string)
        (D.field "misto_en" D.string)
        (D.field "misto_gps" decodeMaybeGps)
        (D.field "misto_id" D.int)
        |> D_.apply (D.field "misto_telefon" D.string)
        |> D_.apply (D.field "misto_website" D.string)
        |> D_.apply (D.field "nazev_cz" D.string)
        |> D_.apply (D.field "nazev_en" D.string)
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
                        D.fail ("Unknown type " ++ v ++ ".")
            )


decodeMaybeGps : D.Decoder (Maybe Gps)
decodeMaybeGps =
    D.oneOf
        [ D.string
            |> D.andThen
                (\v ->
                    if v == "" then
                        D.succeed Nothing

                    else
                        D.fail "GPS coordinates are not empty."
                )
        , Gps.decode
            |> D.map Just
        ]


decodePosix : D.Decoder Time.Posix
decodePosix =
    Iso8601.decoder
        |> D.map
            (\v ->
                Time.millisToPosix (Time.posixToMillis v - timeOffset)
            )



--


timeOffset : Int
timeOffset =
    2


filmLink : Locale -> Int -> String
filmLink locale a =
    case locale of
        English ->
            "https://www.kviff.com/en/programme/film/57/" ++ Url.percentEncode (String.fromInt a)

        Czech ->
            "https://www.kviff.com/cs/program/film/57/" ++ Url.percentEncode (String.fromInt a)



--


dataToEvents : Data -> List Event
dataToEvents a =
    let
        filmToEvents : Film -> List Event
        filmToEvents b =
            b.screenings |> List.map (screeningToEvent b)

        screeningToEvent : Film -> Screening -> Event
        screeningToEvent film b =
            let
                place : Place
                place =
                    { id = b.theatreId
                    , name = b.theatreName

                    --
                    , address = b.theatreAddress
                    , gps = Just b.theatreGps

                    --
                    , phone = ""
                    , website = ""
                    }

                note : Localized String
                note =
                    let
                        internalNote : List String
                        internalNote =
                            String.split "\n" film.internalNote
                                |> List.filter (String.trim >> String.isEmpty >> not)
                    in
                    Localized
                        (b.code
                            :: String.fromInt film.year
                            :: film.country.en
                            :: []
                            |> String.join " | "
                        )
                        (b.code
                            :: String.fromInt film.year
                            :: film.country.cz
                            :: internalNote
                            |> String.join " | "
                        )
            in
            { id = Nothing
            , filmId = Just film.id
            , type_ = Screening_

            --
            , name = Localized film.name film.name
            , image = film.images |> List.head
            , description = film.annotation
            , note = note

            --
            , timeStart = Just b.timeStart
            , timeEnd = Just (Time.millisToPosix (Time.posixToMillis b.timeStart + (film.duration * 60 * 1000)))

            --
            , place = place
            }
    in
    a.events ++ List.concatMap filmToEvents a.films
