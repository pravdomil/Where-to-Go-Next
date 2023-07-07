module Kviff.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import Html
import Http
import Id
import Kviff.Data
import Kviff.GeoCoordinates
import Kviff.Locale
import Kviff.Model
import Kviff.Msg
import Kviff.Utils.Html
import Kviff.Utils.Theme exposing (..)
import Kviff.Utils.Translation
import Time
import Url


view : Kviff.Model.Model -> Browser.Document Kviff.Msg.Msg
view model =
    { title = Kviff.Utils.Translation.title
    , body =
        [ layout theme [] (viewBody model)
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
    column [ width (fill |> maximum (320 * 2)), spacing 32, padding 16, centerX ]
        [ row [ spacing 8 ]
            [ heading1 theme
                []
                [ text Kviff.Utils.Translation.title
                ]
            , localeChooser
            ]
        , case model.data of
            Ok b ->
                viewEvents model b

            Err b ->
                viewError b
        , el [] none
        , paragraph theme
            [ fontCenter, fontSize 14, fontColor style.fore60 ]
            [ text Kviff.Utils.Translation.footer
            ]
        ]


viewError : Kviff.Model.Error -> Element msg
viewError a =
    paragraph theme
        [ fontSize 14, fontColor style.fore70 ]
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
    let
        imgContain : Element msg
        imgContain =
            html
                (Html.node "style"
                    []
                    [ Html.text "img { object-fit: contain; }"
                    ]
                )
    in
    column [ width fill, inFront imgContain, spacing 20 ]
        (Dict.Any.toList a.events
            |> List.sortBy (\( _, x ) -> Time.posixToMillis (Kviff.Data.eventTime x))
            |> List.map (viewEvent model a)
        )


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

        films : List Kviff.Data.Film
        films =
            List.filterMap (\x -> Dict.Any.get Id.toString x.filmId data.films) a.films

        categories : List Kviff.Data.Category
        categories =
            List.foldl (\x acc -> Dict.Any.union Id.toString x.categories acc) Dict.Any.empty films
                |> Dict.Any.keys
                |> List.filterMap (\x -> Dict.Any.get Id.toString x data.categories)

        onlyOneFilm : Maybe Kviff.Data.Film
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
                , List.map (\x -> Kviff.Locale.localize model.locale x.name) films
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
    in
    column [ width fill, spacing 4 ]
        (paragraph theme
            [ fontSemiBold ]
            [ textEllipsis [] name
            ]
            :: paragraph theme
                [ fontSize 14, fontColor style.fore70 ]
                (List.intersperse (text " – ")
                    (List.filterMap identity
                        [ Just (text (Kviff.Utils.Translation.date Kviff.Data.timeZone a.time))
                        , Just (text (Kviff.Utils.Translation.time Kviff.Data.timeZone a.time))
                        , Just (text (Kviff.Utils.Translation.duration (List.foldl (\x acc -> acc + (60 * 1000 * x.duration)) 0 films)))
                        , Maybe.map
                            (\x ->
                                newTabLink theme
                                    []
                                    { label = text (Kviff.Locale.localize model.locale x.name)
                                    , url = Kviff.GeoCoordinates.mapyCzLink x.coordinates
                                    }
                            )
                            place
                        , Just (text (String.join ", " (List.map (\x -> Kviff.Locale.localize model.locale x.name) categories)))
                        , case onlyOneFilm of
                            Just b ->
                                Just
                                    (newTabLink theme
                                        []
                                        { label = text "CSFD"
                                        , url = Kviff.Data.csfdLink b
                                        }
                                    )

                            Nothing ->
                                Nothing
                        ]
                    )
                )
            :: (case films of
                    [] ->
                        [ text "…"
                        ]

                    _ ->
                        List.map
                            (\x ->
                                paragraph theme
                                    [ spacing 2, fontSize 14 ]
                                    (case onlyOneFilm of
                                        Just _ ->
                                            [ text (Kviff.Utils.Html.stripTags (Kviff.Locale.localize model.locale x.description))
                                            ]

                                        Nothing ->
                                            [ newTabLink theme
                                                []
                                                { label = text (Kviff.Locale.localize model.locale x.name)
                                                , url = Kviff.Data.csfdLink x
                                                }
                                            , text ": "
                                            , text (Kviff.Utils.Html.stripTags (Kviff.Locale.localize model.locale x.description))
                                            ]
                                    )
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
