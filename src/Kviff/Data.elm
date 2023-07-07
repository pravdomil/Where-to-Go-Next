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
    { name : Kviff.Locale.Localized String
    , type_ : ScreeningType
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
    , authors : FilmAuthors
    , year : Int
    , duration : Int
    , country : Kviff.Locale.Localized String
    , categories : Dict.Any.Dict (Id.Id Category) ()

    --
    , internalNote : String
    }



--


type alias FilmAuthors =
    { production : List String
    , producer : String
    , directors : List String
    , cast : String
    , dop : String
    , screenplay : String
    , artDirector : String
    , cut : String
    , music : String
    , sound : String
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


timeOffset : Int
timeOffset =
    60 * 2


timeZone : Time.Zone
timeZone =
    Time.customZone timeOffset []


filmLink : Kviff.Locale.Locale -> Id.Id Film -> String
filmLink locale a =
    case locale of
        Kviff.Locale.English ->
            "https://www.kviff.com/en/programme/film/57/" ++ Url.percentEncode (Id.toString a)

        Kviff.Locale.Czech ->
            "https://www.kviff.com/cs/program/film/57/" ++ Url.percentEncode (Id.toString a)



--


decoder : Json.Decode.Decoder Data
decoder =
    Json.Decode.map4
        Data
        eventsDecoder
        filmsDecoder
        categoriesDecoder
        placesDecoder


eventsDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Event) Event)
eventsDecoder =
    Json.Decode.field "sekce"
        (Json.Decode.list
            (Json.Decode.field "subsekce"
                (Json.Decode.list
                    (Json.Decode.field "film"
                        (Json.Decode.list
                            (Json.Decode.field "screenings"
                                (Json.Decode.field "screening"
                                    (Json.Decode.list
                                        (Json.Decode.map (Tuple.mapBoth Id.toAny Screening_) screeningDecoder)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString (List.concat (List.concat (List.concat x))))


screeningDecoder : Json.Decode.Decoder ( Id.Id Screening, Screening )
screeningDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "code" (Json.Decode.map Id.fromString Json.Decode.string))
        (Json.Decode.map4
            Screening
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "title_en" Json.Decode.string)
                (Json.Decode.field "title_cz" Json.Decode.string)
            )
            (Json.Decode.field "type" screeningTypeDecoder)
            (Json.Decode.field "films"
                (Json.Decode.list
                    (Json.Decode.map2
                        (\x x2 -> { filmId = x, note = x2 })
                        (Json.Decode.field "id_film" idDecoder)
                        (Json.Decode.map2 Kviff.Locale.Localized
                            (Json.Decode.field "screening_note_en" Json.Decode.string)
                            (Json.Decode.field "screening_note_cz" Json.Decode.string)
                        )
                    )
                )
            )
            (Json.Decode.field "timestamp" posixDecoder)
        )


screeningTypeDecoder : Json.Decode.Decoder ScreeningType
screeningTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case x of
                    "OFFICIAL" ->
                        Json.Decode.succeed Official

                    _ ->
                        Json.Decode.fail "Unknown screening type."
            )



--


filmsDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Film) Film)
filmsDecoder =
    Json.Decode.field "sekce"
        (Json.Decode.list
            (Json.Decode.field "subsekce"
                (Json.Decode.list
                    (Json.Decode.field "film"
                        (Json.Decode.list
                            filmDecoder
                        )
                    )
                )
            )
        )
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString (List.concat (List.concat x)))


filmDecoder : Json.Decode.Decoder ( Id.Id Film, Film )
filmDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id_film" idDecoder)
        (map10
            Film
            (Json.Decode.field "nazev_orig" Json.Decode.string)
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "nazev_en" Json.Decode.string)
                (Json.Decode.field "nazev_cz" Json.Decode.string)
            )
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "film_en" Json.Decode.string)
                (Json.Decode.field "film_cz" Json.Decode.string)
            )
            (Json.Decode.field "director_web_images" (Json.Decode.list (Json.Decode.field "image" (Json.Decode.field "filename" Json.Decode.string))))
            filmAuthorsDecoder
            (Json.Decode.field "rok" Json.Decode.int)
            (Json.Decode.field "delka" Json.Decode.int)
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "zeme_en" Json.Decode.string)
                (Json.Decode.field "zeme_cz" Json.Decode.string)
            )
            (Json.Decode.map (Dict.Any.fromList Id.toString) (Json.Decode.field "id_sekce" (Json.Decode.map (\x -> [ ( x, () ) ]) idDecoder)))
            (Json.Decode.field "poznamky" Json.Decode.string)
        )


filmAuthorsDecoder : Json.Decode.Decoder FilmAuthors
filmAuthorsDecoder =
    map10
        FilmAuthors
        (Json.Decode.field "produkce" (Json.Decode.list (Json.Decode.field "jmeno" Json.Decode.string)))
        (Json.Decode.field "producent" Json.Decode.string)
        (Json.Decode.field "directors" (Json.Decode.map List.concat (Json.Decode.list (Json.Decode.field "director" (Json.Decode.list Json.Decode.string)))))
        (Json.Decode.field "hraji" Json.Decode.string)
        (Json.Decode.field "kamera" Json.Decode.string)
        (Json.Decode.field "scenar" Json.Decode.string)
        (Json.Decode.field "vytvarnik" Json.Decode.string)
        (Json.Decode.field "strih" Json.Decode.string)
        (Json.Decode.field "hudba" Json.Decode.string)
        (Json.Decode.field "zvuk" Json.Decode.string)



--


categoriesDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Category) Category)
categoriesDecoder =
    Json.Decode.field "sekce"
        (Json.Decode.list categoryDecoder)
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString x)


categoryDecoder : Json.Decode.Decoder ( Id.Id Category, Category )
categoryDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id_sekce" idDecoder)
        (Json.Decode.map3
            Category
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "name_en" Json.Decode.string)
                (Json.Decode.field "name_cz" Json.Decode.string)
            )
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "popis_en" Json.Decode.string)
                (Json.Decode.field "popis_cz" Json.Decode.string)
            )
            (Json.Decode.field "k_order" Json.Decode.int)
        )



--


placesDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Place) Place)
placesDecoder =
    Json.Decode.field "sekce"
        (Json.Decode.list
            (Json.Decode.field "subsekce"
                (Json.Decode.list
                    (Json.Decode.field "film"
                        (Json.Decode.list
                            (Json.Decode.field "screenings"
                                (Json.Decode.field "screening"
                                    (Json.Decode.list placeDecoder)
                                )
                            )
                        )
                    )
                )
            )
        )
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString (List.concat (List.concat (List.concat x))))


placeDecoder : Json.Decode.Decoder ( Id.Id Place, Place )
placeDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "theatre_misto_id" idDecoder)
        (Json.Decode.map4
            Place
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "theatre_en" Json.Decode.string)
                (Json.Decode.field "theatre_cz" Json.Decode.string)
            )
            (Json.Decode.field "theatre_misto_adresa" Json.Decode.string)
            (Json.Decode.field "theatre_misto_gps" Kviff.Gps.decoder)
            (Json.Decode.field "theatre_code" Json.Decode.string)
        )



--


decodeFilmImage : Json.Decode.Decoder String
decodeFilmImage =
    Json.Decode.map2
        (\v1 v2 ->
            "https://www.kviff.com/en/image/film/" ++ Url.percentEncode (String.fromInt v1) ++ "/" ++ Url.percentEncode v2
        )
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "validationCode" Json.Decode.string)



--


posixDecoder : Json.Decode.Decoder Time.Posix
posixDecoder =
    Json.Decode.map
        (\x -> Time.millisToPosix (Time.posixToMillis x - (1000 * 60 * timeOffset)))
        Iso8601.decoder


idDecoder : Json.Decode.Decoder (Id.Id a)
idDecoder =
    Json.Decode.map (\x -> Id.fromString (String.fromInt x)) Json.Decode.int


map9 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 =
    Json.Decode.map2
        (\x x2 -> x x2)
        (Json.Decode.map8 fn a1 a2 a3 a4 a5 a6 a7 a8)
        a9


map10 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
    Json.Decode.map2
        (\x x2 -> x x2)
        (map9 fn a1 a2 a3 a4 a5 a6 a7 a8 a9)
        a10
