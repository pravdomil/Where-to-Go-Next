module Kviff.Data exposing (..)

import Dict.Any
import Id
import Iso8601
import Json.Decode
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
    { type_ : ScreeningType
    , name : Kviff.Locale.Localized String
    , code : String
    , films :
        List
            { filmId : Id.Id Film
            , note : Kviff.Locale.Localized String
            }
    , time : Time.Posix
    }



--


type ScreeningType
    = Official



--


type alias Film =
    { name : String
    , localizedName : Kviff.Locale.Localized String
    , description : Kviff.Locale.Localized String
    , images : List String

    --
    , directors : List String
    , production : String
    , cast : String
    , dop : String
    , screenplay : String
    , artDirector : String
    , cut : String
    , music : String
    , sound : String

    --
    , year : Int
    , duration : Int
    , country : Kviff.Locale.Localized String

    --
    , internalNote : String
    }



--


type alias Category =
    { name : Kviff.Locale.Localized String
    , description : Kviff.Locale.Localized String
    , order : Int
    }



--


type alias Place =
    { name : Kviff.Locale.Localized String
    , address : String
    , gps : Kviff.Gps.Gps
    , code : String
    }



--


decodeFilms : Json.Decode.Decoder (List Film)
decodeFilms =
    Json.Decode.field "sekce" (Json.Decode.list (Json.Decode.field "subsekce" (Json.Decode.list (Json.Decode.field "film" (Json.Decode.list decodeFilm)))))
        |> Json.Decode.map (List.concat >> List.concat)


decodeFilm : Json.Decode.Decoder Film
decodeFilm =
    Json.Decode.map8
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
        (Json.Decode.field "id_film" Json.Decode.int)
        (Json.Decode.field "nazev_en" Json.Decode.string)
        (Json.Decode.field "nazev_cz" Json.Decode.string)
        (Json.Decode.field "nazev_orig" Json.Decode.string)
        (Json.Decode.field "author" Json.Decode.string)
        (Json.Decode.field "anotace_en" Json.Decode.string)
        (Json.Decode.field "anotace_cz" Json.Decode.string)
        (Json.Decode.field "film_en" Json.Decode.string)
        |> D_.apply (Json.Decode.field "film_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "doplnujici_text_en" Json.Decode.string)
        |> D_.apply (Json.Decode.field "doplnujici_text_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "poznamky" Json.Decode.string)
        |> D_.apply (Json.Decode.field "rok" Json.Decode.int)
        |> D_.apply (Json.Decode.field "delka" Json.Decode.int)
        |> D_.apply (Json.Decode.field "zeme_en" Json.Decode.string)
        |> D_.apply (Json.Decode.field "zeme_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "screenings" (Json.Decode.field "screening" (Json.Decode.list decodeFilmScreening)))
        |> D_.apply (Json.Decode.field "film_web_images" (Json.Decode.field "image" (Json.Decode.list decodeFilmImage)))


decodeFilmScreening : Json.Decode.Decoder Screening
decodeFilmScreening =
    Json.Decode.map8
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
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "title_en" Json.Decode.string)
        (Json.Decode.field "title_cz" Json.Decode.string)
        (Json.Decode.field "timestamp" decodePosix)
        (Json.Decode.field "theatre_misto_id" Json.Decode.int)
        (Json.Decode.field "theatre_code" Json.Decode.string)
        (Json.Decode.field "theatre_en" Json.Decode.string)
        (Json.Decode.field "theatre_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "theatre_misto_adresa" Json.Decode.string)
        |> D_.apply (Json.Decode.field "theatre_misto_gps" Gps.decode)


decodeFilmImage : Json.Decode.Decoder String
decodeFilmImage =
    Json.Decode.map2
        (\v1 v2 ->
            "https://www.kviff.com/en/image/film/" ++ Url.percentEncode (String.fromInt v1) ++ "/" ++ Url.percentEncode v2
        )
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "validationCode" Json.Decode.string)


decodeEvents : Json.Decode.Decoder (List Event)
decodeEvents =
    Json.Decode.field "typ" (Json.Decode.list (Json.Decode.field "den" (Json.Decode.list (Json.Decode.field "akce" (Json.Decode.list decodeEvent)))))
        |> Json.Decode.map (List.concat >> List.concat)


decodeEvent : Json.Decode.Decoder Event
decodeEvent =
    Json.Decode.map8
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
        (Json.Decode.field "cas_do" (D_.maybe decodePosix))
        (Json.Decode.field "cas_od" (D_.maybe decodePosix))
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "misto_adresa" Json.Decode.string)
        (Json.Decode.field "misto_cz" Json.Decode.string)
        (Json.Decode.field "misto_en" Json.Decode.string)
        (Json.Decode.field "misto_gps" decodeMaybeGps)
        (Json.Decode.field "misto_id" Json.Decode.int)
        |> D_.apply (Json.Decode.field "misto_telefon" Json.Decode.string)
        |> D_.apply (Json.Decode.field "misto_website" Json.Decode.string)
        |> D_.apply (Json.Decode.field "nazev_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "nazev_en" Json.Decode.string)
        |> D_.apply (Json.Decode.field "popis_cz" Json.Decode.string)
        |> D_.apply (Json.Decode.field "popis_en" Json.Decode.string)
        |> D_.apply (Json.Decode.field "typ" decodeEventType)


decodeEventType : Json.Decode.Decoder EventType
decodeEventType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\v ->
                case v of
                    "Akce" ->
                        Json.Decode.succeed Event_

                    "Denně" ->
                        Json.Decode.succeed Daily

                    "KVIFF Talk" ->
                        Json.Decode.succeed Talk

                    "Výstava" ->
                        Json.Decode.succeed Exhibition

                    "Restaurace" ->
                        Json.Decode.succeed Restaurant

                    _ ->
                        Json.Decode.fail ("Unknown type " ++ v ++ ".")
            )


decodeMaybeGps : Json.Decode.Decoder (Maybe Gps)
decodeMaybeGps =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\v ->
                    if v == "" then
                        Json.Decode.succeed Nothing

                    else
                        Json.Decode.fail "GPS coordinates are not empty."
                )
        , Gps.decode
            |> Json.Decode.map Just
        ]



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



--


decodePosix : Json.Decode.Decoder Time.Posix
decodePosix =
    Iso8601.decoder
        |> Json.Decode.map
            (\v ->
                Time.millisToPosix (Time.posixToMillis v - (1000 * 60 * timeOffset))
            )


idDecoder : Json.Decode.Decoder (Id.Id a)
idDecoder =
    Json.Decode.map (\x -> Id.fromString (String.fromInt x)) Json.Decode.int
