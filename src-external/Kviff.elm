module Kviff exposing (..)

import Dict.Any
import Festival.Data
import Festival.GeoCoordinates
import Festival.Locale
import Http
import Http.Resolver
import Id
import Iso8601
import Json.Decode
import Task
import Time
import Url


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
    request dataDecoder "catalog"



--


timeOffset : Int
timeOffset =
    60 * 2


dataDecoder : Json.Decode.Decoder Festival.Data.Data
dataDecoder =
    let
        computeScreeningDuration : Festival.Data.Data -> Festival.Data.Screening -> Festival.Data.Screening
        computeScreeningDuration data b =
            let
                films : List Festival.Data.Film
                films =
                    List.filterMap (\x -> Dict.Any.get Id.toString x.filmId data.films) b.films
            in
            { b | duration = List.foldl (\x acc -> acc + x.duration) 0 films * 60 * 1000 }

        normalize : Festival.Data.Data -> Festival.Data.Data
        normalize data =
            { data
                | events =
                    Dict.Any.map
                        (\_ x ->
                            case x of
                                Festival.Data.Screening_ x2 ->
                                    Festival.Data.Screening_ (computeScreeningDuration data x2)
                        )
                        data.events
            }
    in
    Json.Decode.map4
        Festival.Data.Data
        eventsDecoder
        filmsDecoder
        categoriesDecoder
        placesDecoder
        |> Json.Decode.map normalize


eventsDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Festival.Data.Event) Festival.Data.Event)
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
                                        (Json.Decode.map (Tuple.mapBoth Id.toAny Festival.Data.Screening_) screeningDecoder)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString (List.concat (List.concat (List.concat x))))


screeningDecoder : Json.Decode.Decoder ( Id.Id Festival.Data.Screening, Festival.Data.Screening )
screeningDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "code" (Json.Decode.map Id.fromString Json.Decode.string))
        (Json.Decode.map6
            Festival.Data.Screening
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "title_en" maybeEmptyStringDecoder)
                (Json.Decode.field "title_cz" maybeEmptyStringDecoder)
            )
            (Json.Decode.field "type" screeningTypeDecoder)
            (Json.Decode.field "theatre_misto_id" idDecoder)
            (Json.Decode.field "films"
                (Json.Decode.list
                    (Json.Decode.map2
                        Festival.Data.ScreeningFilm
                        (Json.Decode.field "id_film" idDecoder)
                        (Json.Decode.map2 Festival.Locale.Localized
                            (Json.Decode.field "screening_note_en" Json.Decode.string)
                            (Json.Decode.field "screening_note_cz" Json.Decode.string)
                        )
                    )
                )
            )
            (Json.Decode.field "timestamp" posixDecoder)
            (Json.Decode.succeed 0)
        )


screeningTypeDecoder : Json.Decode.Decoder Festival.Data.ScreeningType
screeningTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case x of
                    "OFFICIAL" ->
                        Json.Decode.succeed Festival.Data.Official

                    "INVITATION_ONLY" ->
                        Json.Decode.succeed Festival.Data.InvitationOnly

                    _ ->
                        Json.Decode.fail "Unknown screening type."
            )



--


filmsDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Festival.Data.Film) Festival.Data.Film)
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


filmDecoder : Json.Decode.Decoder ( Id.Id Festival.Data.Film, Festival.Data.Film )
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
            Festival.Data.Film
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "nazev_en" Json.Decode.string)
                (Json.Decode.field "nazev_cz" Json.Decode.string)
            )
            (Json.Decode.field "nazev_orig" Json.Decode.string)
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "film_en" Json.Decode.string)
                (Json.Decode.field "film_cz" Json.Decode.string)
            )
            (Json.Decode.field "film_web_images" (Json.Decode.field "image" (Json.Decode.list imageUrlDecoder)))
            filmAuthorsDecoder
            (Json.Decode.field "rok" Json.Decode.int)
            (Json.Decode.field "delka" Json.Decode.int)
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "zeme_en" Json.Decode.string)
                (Json.Decode.field "zeme_cz" Json.Decode.string)
            )
            (Json.Decode.map (Dict.Any.fromList Id.toString) (Json.Decode.field "id_sekce" (Json.Decode.map (\x -> [ ( x, () ) ]) idDecoder)))
            (Json.Decode.field "poznamky" Json.Decode.string)
        )


filmAuthorsDecoder : Json.Decode.Decoder Festival.Data.FilmAuthors
filmAuthorsDecoder =
    map10
        Festival.Data.FilmAuthors
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


categoriesDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Festival.Data.Category) Festival.Data.Category)
categoriesDecoder =
    Json.Decode.field "sekce"
        (Json.Decode.list categoryDecoder)
        |> Json.Decode.map (\x -> Dict.Any.fromList Id.toString x)


categoryDecoder : Json.Decode.Decoder ( Id.Id Festival.Data.Category, Festival.Data.Category )
categoryDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id_sekce" idDecoder)
        (Json.Decode.map3
            Festival.Data.Category
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "name_en" Json.Decode.string)
                (Json.Decode.field "name_cz" Json.Decode.string)
            )
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "popis_en" Json.Decode.string)
                (Json.Decode.field "popis_cz" Json.Decode.string)
            )
            (Json.Decode.field "k_order" Json.Decode.int)
        )



--


placesDecoder : Json.Decode.Decoder (Dict.Any.Dict (Id.Id Festival.Data.Place) Festival.Data.Place)
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


placeDecoder : Json.Decode.Decoder ( Id.Id Festival.Data.Place, Festival.Data.Place )
placeDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "theatre_misto_id" idDecoder)
        (Json.Decode.map4
            Festival.Data.Place
            (Json.Decode.map2 Festival.Locale.Localized
                (Json.Decode.field "theatre_en" Json.Decode.string)
                (Json.Decode.field "theatre_cz" Json.Decode.string)
            )
            (Json.Decode.field "theatre_misto_adresa" Json.Decode.string)
            (Json.Decode.field "theatre_misto_gps" Festival.GeoCoordinates.decoder)
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
