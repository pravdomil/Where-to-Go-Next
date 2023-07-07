module Kviff.Data exposing (..)

import Dict.Any
import Id
import Iso8601
import Json.Decode
import Kviff.GeoCoordinates
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


eventTime : Event -> Time.Posix
eventTime a =
    case a of
        Screening_ b ->
            b.time


eventDuration : Event -> Int
eventDuration a =
    case a of
        Screening_ b ->
            b.duration



--


type alias Screening =
    { name : Kviff.Locale.Localized (Maybe String)
    , type_ : ScreeningType
    , place : Id.Id Place
    , films : List ScreeningFilm
    , time : Time.Posix
    , duration : Int
    }



--


type alias ScreeningFilm =
    { filmId : Id.Id Film
    , note : Kviff.Locale.Localized String
    }



--


type ScreeningType
    = Official
    | InvitationOnly



--


type alias Film =
    { name : Kviff.Locale.Localized String
    , originalName : String
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
    , coordinates : Kviff.GeoCoordinates.GeoCoordinates
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


csfdLink : Film -> String
csfdLink a =
    "https://www.csfd.cz/hledat/?q=" ++ Url.percentEncode a.originalName



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
        (Json.Decode.map6
            Screening
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "title_en" maybeEmptyStringDecoder)
                (Json.Decode.field "title_cz" maybeEmptyStringDecoder)
            )
            (Json.Decode.field "type" screeningTypeDecoder)
            (Json.Decode.field "theatre_misto_id" idDecoder)
            (Json.Decode.field "films"
                (Json.Decode.list
                    (Json.Decode.map2
                        ScreeningFilm
                        (Json.Decode.field "id_film" idDecoder)
                        (Json.Decode.map2 Kviff.Locale.Localized
                            (Json.Decode.field "screening_note_en" Json.Decode.string)
                            (Json.Decode.field "screening_note_cz" Json.Decode.string)
                        )
                    )
                )
            )
            (Json.Decode.field "timestamp" posixDecoder)
            (Json.Decode.succeed 0)
        )


screeningTypeDecoder : Json.Decode.Decoder ScreeningType
screeningTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case x of
                    "OFFICIAL" ->
                        Json.Decode.succeed Official

                    "INVITATION_ONLY" ->
                        Json.Decode.succeed InvitationOnly

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
    let
        imageUrlDecoder : Json.Decode.Decoder String
        imageUrlDecoder =
            Json.Decode.map2
                (\x x2 ->
                    "https://www.kviff.com/en/image/film/" ++ Url.percentEncode (String.fromInt x) ++ "/" ++ Url.percentEncode x2
                )
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "validationCode" Json.Decode.string)
    in
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id_film" idDecoder)
        (map10
            Film
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "nazev_en" Json.Decode.string)
                (Json.Decode.field "nazev_cz" Json.Decode.string)
            )
            (Json.Decode.field "nazev_orig" Json.Decode.string)
            (Json.Decode.map2 Kviff.Locale.Localized
                (Json.Decode.field "film_en" Json.Decode.string)
                (Json.Decode.field "film_cz" Json.Decode.string)
            )
            (Json.Decode.field "director_web_images" (Json.Decode.field "image" (Json.Decode.list imageUrlDecoder)))
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
        (Json.Decode.field "directors" (Json.Decode.field "director" (Json.Decode.list Json.Decode.string)))
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
            (Json.Decode.field "theatre_misto_gps" Kviff.GeoCoordinates.decoder)
            (Json.Decode.field "theatre_code" Json.Decode.string)
        )



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


maybeEmptyStringDecoder : Json.Decode.Decoder (Maybe String)
maybeEmptyStringDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case x of
                    "" ->
                        Json.Decode.succeed Nothing

                    _ ->
                        Json.Decode.succeed (Just x)
            )
