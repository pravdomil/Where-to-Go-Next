module Kviff.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import Html
import Http
import Id
import Kviff.Data
import Kviff.ElementId
import Kviff.Locale
import Kviff.Model
import Kviff.Msg
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
                        { label = text "CZ"
                        , active = model.locale == Kviff.Locale.Czech
                        , onPress = Just (Kviff.Msg.LocaleRequested Kviff.Locale.Czech)
                        }

                Kviff.Locale.Czech ->
                    button theme
                        []
                        { label = text "EN"
                        , active = model.locale == Kviff.Locale.English
                        , onPress = Just (Kviff.Msg.LocaleRequested Kviff.Locale.English)
                        }
    in
    column [ spacing 32, padding 16, width (fill |> maximum (320 * 2)), centerX ]
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
viewError b =
    paragraph theme
        [ fontSize 14, fontColor style.fore70 ]
        [ case b of
            Kviff.Model.Loading ->
                text "Loading..."

            Kviff.Model.HttpError c ->
                case c of
                    Http.Timeout ->
                        text "There is a network error. Try reload."

                    Http.NetworkError ->
                        text "There is a network error. Try reload."

                    _ ->
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
    column [ inFront imgContain, spacing 20 ]
        (Dict.Any.toList a.events
            |> List.sortBy (\( _, x ) -> Time.posixToMillis (Kviff.Data.eventTime x))
            |> List.map (viewEvent model)
        )


viewEvent : Kviff.Model.Model -> ( Id.Id Kviff.Data.Event, Kviff.Data.Event ) -> Element Kviff.Msg.Msg
viewEvent model ( id, a ) =
    column [ spacing 4, Kviff.ElementId.toId (Kviff.ElementId.Event id) ]
        [ column [ spacing 2 ]
            [ heading2 theme
                [ fontSemiBold ]
                [ text (Kviff.Locale.localize model.locale a.name)
                ]
            , paragraph theme
                [ fontSize 14, fontColor style.fore70 ]
                ([ case a.type_ of
                    Api.Screening_ ->
                        Nothing

                    _ ->
                        Just (text (Translation.eventType a.type_))
                 , a.startTime
                    |> Maybe.map (\v -> text (Translation.time Api.timeZone v))
                 , Maybe.map2
                    (\start end ->
                        text (Translation.duration (Time.posixToMillis end - Time.posixToMillis start))
                    )
                    a.startTime
                    a.endTime
                 , Just
                    (case a.place.gps of
                        Just b ->
                            newTabLink []
                                { label = text (Api.localize model.locale a.place.name)
                                , url =
                                    "https://mapy.cz/?z=16&y="
                                        ++ Url.percentEncode (String.fromFloat b.lat)
                                        ++ "&x="
                                        ++ Url.percentEncode (String.fromFloat b.lon)
                                        ++ "&q="
                                        ++ Url.percentEncode (String.fromFloat b.lat ++ " " ++ String.fromFloat b.lon)
                                }

                        Nothing ->
                            text (Api.localize model.locale a.place.name)
                    )
                 , case a.filmId of
                    Just b ->
                        Just
                            (newTabLink []
                                { label = text "Info"
                                , url = Api.filmLink model.locale b
                                }
                            )

                    Nothing ->
                        Nothing
                 , case a.filmId of
                    Just _ ->
                        Just
                            (newTabLink []
                                { label = text "CSFD"
                                , url = "https://www.csfd.cz/hledat/?q=" ++ Url.percentEncode a.name.cz
                                }
                            )

                    Nothing ->
                        Nothing
                 ]
                    |> List.filterMap identity
                    |> List.intersperse (text " – ")
                )
            ]
        , case a.image of
            Just b ->
                image [ width (px 128), height (px 128), alignRight ]
                    { description = ""
                    , src = b
                    }

            Nothing ->
                none
        , p [ fontSize 14, fontColor grey3 ]
            [ text
                (Utils.Html.stripTags (Api.localize model.locale a.description)
                    |> Result.withDefault (Api.localize model.locale a.description)
                )
            ]
        , p [ fontSize 10, fontColor grey4 ]
            [ text (Api.localize model.locale a.note)
            ]
        ]
