module Kviff.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import Html
import Http
import Id
import Kviff.Data
import Kviff.ElementId
import Kviff.GeoCoordinates
import Kviff.Locale
import Kviff.Model
import Kviff.Msg
import Kviff.Utils.Html
import Kviff.Utils.Theme exposing (..)
import Kviff.Utils.Translation
import Time


view : Kviff.Model.Model -> Browser.Document Kviff.Msg.Msg
view model =
    { title = Kviff.Utils.Translation.title
    , body =
        [ layout theme [] (viewBody model)
        , Html.node "style" [] [ Html.text "body{background-color:rgb(0,0,0)}" ]
        , Html.node "style" [] [ Html.text "img{object-fit:contain;}" ]
        ]
    }



--


viewBody : Kviff.Model.Model -> Element Kviff.Msg.Msg
viewBody model =
    let
        localeChooser : Element Kviff.Msg.Msg
        localeChooser =
            case model.locale of
                Kviff.Locale.English ->
                    button theme
                        []
                        { label = text (Kviff.Utils.Translation.locale Kviff.Locale.Czech)
                        , active = model.locale == Kviff.Locale.Czech
                        , onPress = Just (Kviff.Msg.LocaleRequested Kviff.Locale.Czech)
                        }

                Kviff.Locale.Czech ->
                    button theme
                        []
                        { label = text (Kviff.Utils.Translation.locale Kviff.Locale.English)
                        , active = model.locale == Kviff.Locale.English
                        , onPress = Just (Kviff.Msg.LocaleRequested Kviff.Locale.English)
                        }
    in
    column [ width (fill |> maximum (320 * 2)), spacing 32, padding 8, centerX ]
        [ row [ width fill, spacing 8 ]
            [ heading1 theme
                [ width fill ]
                [ text Kviff.Utils.Translation.title
                ]
            , localeChooser
            ]
        , case model.data of
            Ok b ->
                viewEvents model b

            Err b ->
                viewError b
        , paragraph theme
            [ spacing 2, fontSize 14, fontColor style.fore50, fontCenter ]
            [ text Kviff.Utils.Translation.footer
            ]
        ]


viewError : Kviff.Model.Error -> Element msg
viewError a =
    paragraph theme
        [ spacing 2, fontSize 14, fontColor style.fore50 ]
        [ case a of
            Kviff.Model.Loading ->
                text "Loading…"

            Kviff.Model.HttpError b ->
                case b of
                    Http.BadUrl _ ->
                        text "Sorry, but application is not available."

                    Http.Timeout ->
                        text "There is a network error. Try reload."

                    Http.NetworkError ->
                        text "There is a network error. Try reload."

                    Http.BadStatus _ ->
                        text "Sorry, but application is not available."

                    Http.BadBody _ ->
                        text "Sorry, but application is not available."
        ]


viewEvents : Kviff.Model.Model -> Kviff.Data.Data -> Element Kviff.Msg.Msg
viewEvents model a =
    column [ width fill, spacing 20 ]
        (Dict.Any.toList a.events
            |> List.sortBy (\( _, x ) -> Time.posixToMillis (Kviff.Data.eventTime x))
            |> List.foldl
                (\( id, x ) ( acc, time ) ->
                    let
                        maybePrependDate : List (Element msg) -> List (Element msg)
                        maybePrependDate b =
                            if Time.toDay Kviff.Data.timeZone (Kviff.Data.eventTime x) == Time.toDay Kviff.Data.timeZone time then
                                b

                            else
                                viewDay (Kviff.Data.eventTime x) :: b
                    in
                    ( viewEvent model a ( id, x ) :: maybePrependDate acc
                    , Kviff.Data.eventTime x
                    )
                )
                ( [], Time.millisToPosix 0 )
            |> Tuple.first
            |> List.reverse
        )


viewDay : Time.Posix -> Element msg
viewDay a =
    heading1 theme
        [ fontSize 48, paddingXY 0 64 ]
        [ text (Kviff.Utils.Translation.date Kviff.Data.timeZone a)
        ]


viewEvent : Kviff.Model.Model -> Kviff.Data.Data -> ( Id.Id Kviff.Data.Event, Kviff.Data.Event ) -> Element Kviff.Msg.Msg
viewEvent model data ( id, a ) =
    case a of
        Kviff.Data.Screening_ b ->
            viewScreening model data ( Id.toAny id, b )


viewScreening : Kviff.Model.Model -> Kviff.Data.Data -> ( Id.Id Kviff.Data.Screening, Kviff.Data.Screening ) -> Element Kviff.Msg.Msg
viewScreening model data ( id, a ) =
    let
        place : Maybe Kviff.Data.Place
        place =
            Dict.Any.get Id.toString a.place data.places

        films : List ( Id.Id Kviff.Data.Film, Kviff.Data.Film )
        films =
            List.filterMap (\x -> Dict.Any.get Id.toString x.filmId data.films |> Maybe.map (\x2 -> ( x.filmId, x2 ))) a.films

        categories : List Kviff.Data.Category
        categories =
            List.foldl (\( _, x ) acc -> Dict.Any.union Id.toString x.categories acc) Dict.Any.empty films
                |> Dict.Any.keys
                |> List.filterMap (\x -> Dict.Any.get Id.toString x data.categories)

        onlyOneFilm : Maybe ( Id.Id Kviff.Data.Film, Kviff.Data.Film )
        onlyOneFilm =
            case films of
                b :: [] ->
                    Just b

                _ ->
                    Nothing

        name : String
        name =
            List.filterMap identity
                [ Kviff.Locale.localize model.locale a.name
                , List.map (\( _, x ) -> Kviff.Locale.localize model.locale x.name) films
                    |> String.join ", "
                    |> emptyStringToNothing
                ]
                |> (\x ->
                        case x of
                            [] ->
                                "Screening"

                            _ ->
                                String.join " – " x
                   )

        endTime : Time.Posix
        endTime =
            Time.millisToPosix (Time.posixToMillis a.time + a.duration)
    in
    column [ width fill, spacing 4, Kviff.ElementId.toId (Kviff.ElementId.Event (Id.toAny id)) ]
        (paragraph theme
            [ fontSemiBold ]
            [ textEllipsis [] name
            ]
            :: paragraph theme
                [ spacing 2, fontSize 14, fontColor style.fore50 ]
                (List.intersperse (text " – ")
                    (List.filterMap identity
                        [ Just (text (Kviff.Utils.Translation.date Kviff.Data.timeZone a.time))
                        , Just (text (Kviff.Utils.Translation.time Kviff.Data.timeZone a.time ++ "–" ++ Kviff.Utils.Translation.time Kviff.Data.timeZone endTime))
                        , Maybe.map
                            (\x ->
                                newTabLink theme
                                    []
                                    { label = text (Kviff.Locale.localize model.locale x.name)
                                    , url = Kviff.GeoCoordinates.mapyCzLink x.coordinates
                                    }
                            )
                            place
                        , Just (text (Id.toString id))
                        , Just (text (String.join ", " (List.map (\x -> Kviff.Locale.localize model.locale x.name) categories)))
                        , Maybe.map
                            (\( x, _ ) ->
                                newTabLink theme
                                    []
                                    { label = text "Info"
                                    , url = Kviff.Data.filmLink model.locale x
                                    }
                            )
                            onlyOneFilm
                        , Maybe.map
                            (\( _, x ) ->
                                newTabLink theme
                                    []
                                    { label = text "CSFD"
                                    , url = Kviff.Data.csfdLink x
                                    }
                            )
                            onlyOneFilm
                        , Maybe.map
                            (\( _, x ) ->
                                newTabLink theme
                                    []
                                    { label = text "IMDb"
                                    , url = Kviff.Data.imdbLink x
                                    }
                            )
                            onlyOneFilm
                        ]
                    )
                )
            :: (case films of
                    [] ->
                        [ text "…"
                        ]

                    _ ->
                        List.concatMap
                            (\( _, x ) ->
                                [ paragraph theme
                                    [ spacing 2, fontSize 14, fontColor style.fore70 ]
                                    (case onlyOneFilm of
                                        Just _ ->
                                            [ column [ alignRight, spacing 8 ]
                                                (List.map
                                                    (\x2 ->
                                                        image [ width (px 128), height (px 72) ]
                                                            { description = Kviff.Locale.localize model.locale x.name
                                                            , src = x2
                                                            }
                                                    )
                                                    x.images
                                                )
                                            , text (Kviff.Utils.Html.stripTags (Kviff.Locale.localize model.locale x.description))
                                            ]

                                        Nothing ->
                                            [ case List.head x.images of
                                                Just x2 ->
                                                    image [ alignRight, width (px 128), height (px 72) ]
                                                        { description = Kviff.Locale.localize model.locale x.name
                                                        , src = x2
                                                        }

                                                Nothing ->
                                                    none
                                            , newTabLink theme
                                                []
                                                { label = text (Kviff.Locale.localize model.locale x.name)
                                                , url = Kviff.Data.csfdLink x
                                                }
                                            , text " "
                                            , text (Kviff.Utils.Html.stripTags (Kviff.Locale.localize model.locale x.description))
                                            ]
                                    )
                                , paragraph theme
                                    [ spacing 2, fontSize 10, fontColor style.fore50 ]
                                    [ text (String.fromInt x.year)
                                    , text " | "
                                    , text (Kviff.Locale.localize model.locale x.country)
                                    , text " | "
                                    , text x.internalNote
                                    ]
                                ]
                            )
                            films
               )
        )



--


emptyStringToNothing : String -> Maybe String
emptyStringToNothing a =
    case a of
        "" ->
            Nothing

        _ ->
            Just a
