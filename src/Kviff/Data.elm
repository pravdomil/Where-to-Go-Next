module Kviff.Data exposing (..)

import Dict.Any
import Id
import Iso8601
import Kviff.Gps
import Kviff.Locale
import Time
import Url


type alias Data =
    { events : Dict.Any.Dict (Id.Id Event) Event
    , films : Dict.Any.Dict (Id.Id Film) Film
    , categories : Dict.Any.Dict (Id.Id Category) Category
    , places : Dict.Any.Dict (Id.Id Place) Place
    }



--


type Event
    = Screening_ Screening



--


type alias Screening =
    { id : Maybe Int
    , filmId : Maybe Int

    --
    , name : Kviff.Locale.Localized String
    , image : Maybe String
    , description : Kviff.Locale.Localized String
    , note : Kviff.Locale.Localized String

    --
    , startTime : Maybe Time.Posix
    , endTime : Maybe Time.Posix

    --
    , place : Place

    -- MORE
    , code : String
    , name : Kviff.Locale.Localized String
    , startTime : Time.Posix

    --
    , theatreId : Int
    , theatreCode : String
    , theatreName : Kviff.Locale.Localized String

    --
    , theatreAddress : String
    , theatreGps : Kviff.Gps.Gps
    }



--


type alias Film =
    { id : Int

    --
    , name : String
    , nameLocalized : Kviff.Locale.Localized String
    , author : String
    , images : List String

    --
    , annotation : Kviff.Locale.Localized String
    , description : Kviff.Locale.Localized String
    , additionalDesc : Kviff.Locale.Localized String
    , internalNote : String

    --
    , year : Int
    , duration : Int
    , country : Kviff.Locale.Localized String

    --
    , screenings : List Screening
    }



--


type alias Place =
    { id : Int
    , name : Kviff.Locale.Localized String

    --
    , address : String
    , gps : Maybe Kviff.Gps.Gps

    --
    , phone : String
    , website : String
    }



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
            , startTime = v4
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
            "https://www.kviff.com/en/image/film/" ++ Url.percentEncode (String.fromInt v1) ++ "/" ++ Url.percentEncode v2
        )
        (D.field "id" D.int)
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
            , startTime = v2
            , endTime = v1

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
                Time.millisToPosix (Time.posixToMillis v - (1000 * 60 * timeOffset))
            )



--


timeOffset : Int
timeOffset =
    60 * 2


timeZone : Time.Zone
timeZone =
    Time.customZone timeOffset []


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
            , name = film.nameLocalized
            , image = film.images |> List.head
            , description = film.annotation
            , note = note

            --
            , startTime = Just b.startTime
            , endTime = Just (Time.millisToPosix (Time.posixToMillis b.startTime + (film.duration * 60 * 1000)))

            --
            , place = place
            }
    in
    a.events ++ List.concatMap filmToEvents a.films
