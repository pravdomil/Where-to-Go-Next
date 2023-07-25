module Festival.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import Festival.Data
import Festival.ElementId
import Festival.GeoCoordinates
import Festival.Locale
import Festival.Model
import Festival.Model.Utils
import Festival.Msg
import Festival.Utils.Html
import Festival.Utils.Theme exposing (..)
import Festival.Utils.Translation
import Html
import Http
import Id
import Time


view : Festival.Model.Model -> Browser.Document Festival.Msg.Msg
view model =
    { title = Festival.Utils.Translation.title
    , body =
        [ layout theme [] (viewBody model)
        , Html.node "style" [] [ Html.text "body{background-color:rgb(0,0,0)}" ]
        , Html.node "style" [] [ Html.text "img{object-fit:contain;}" ]
        ]
    }



--


viewBody : Festival.Model.Model -> Element Festival.Msg.Msg
viewBody model =
    let
        localeChooser : Element Festival.Msg.Msg
        localeChooser =
            case model.locale of
                Festival.Locale.English ->
                    button theme
                        []
                        { label = text (Festival.Utils.Translation.locale Festival.Locale.Czech)
                        , active = model.locale == Festival.Locale.Czech
                        , onPress = Just (Festival.Msg.LocaleRequested Festival.Locale.Czech)
                        }

                Festival.Locale.Czech ->
                    button theme
                        []
                        { label = text (Festival.Utils.Translation.locale Festival.Locale.English)
                        , active = model.locale == Festival.Locale.English
                        , onPress = Just (Festival.Msg.LocaleRequested Festival.Locale.English)
                        }
    in
    column [ width (fill |> maximum (320 * 2)), spacing 32, padding 8, centerX ]
        [ row [ width fill, spacing 8 ]
            [ heading1 theme
                [ width fill ]
                [ text Festival.Utils.Translation.title
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
            [ text Festival.Utils.Translation.footer
            ]
        ]


viewError : Festival.Model.Error -> Element msg
viewError a =
    paragraph theme
        [ spacing 2, fontSize 14, fontColor style.fore50 ]
        [ case a of
            Festival.Model.Loading ->
                text "Loading…"

            Festival.Model.HttpError b ->
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


viewEvents : Festival.Model.Model -> Festival.Data.Data -> Element Festival.Msg.Msg
viewEvents model a =
    column [ width fill, spacing 20 ]
        (Dict.Any.toList a.events
            |> Festival.Model.Utils.sortEvents
            |> List.foldl
                (\( id, x ) ( acc, time ) ->
                    let
                        maybePrependDate : List (Element msg) -> List (Element msg)
                        maybePrependDate b =
                            if Time.toDay Festival.Data.timeZone (Festival.Data.eventTime x) == Time.toDay Festival.Data.timeZone time then
                                b

                            else
                                viewDay (Festival.Data.eventTime x) :: b
                    in
                    ( viewEvent model a ( id, x ) :: maybePrependDate acc
                    , Festival.Data.eventTime x
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
        [ text (Festival.Utils.Translation.date Festival.Data.timeZone a)
        ]


viewEvent : Festival.Model.Model -> Festival.Data.Data -> ( Id.Id Festival.Data.Event, Festival.Data.Event ) -> Element Festival.Msg.Msg
viewEvent model data ( id, a ) =
    case a of
        Festival.Data.Screening_ b ->
            viewScreening model data ( Id.toAny id, b )


viewScreening : Festival.Model.Model -> Festival.Data.Data -> ( Id.Id Festival.Data.Screening, Festival.Data.Screening ) -> Element Festival.Msg.Msg
viewScreening model data ( id, a ) =
    let
        place : Maybe Festival.Data.Place
        place =
            Dict.Any.get Id.toString a.place data.places

        films : List ( Id.Id Festival.Data.Film, Festival.Data.Film )
        films =
            List.filterMap (\x -> Dict.Any.get Id.toString x.filmId data.films |> Maybe.map (\x2 -> ( x.filmId, x2 ))) a.films

        categories : List Festival.Data.Category
        categories =
            List.foldl (\( _, x ) acc -> Dict.Any.union Id.toString x.categories acc) Dict.Any.empty films
                |> Dict.Any.keys
                |> List.filterMap (\x -> Dict.Any.get Id.toString x data.categories)

        onlyOneFilm : Maybe ( Id.Id Festival.Data.Film, Festival.Data.Film )
        onlyOneFilm =
            case films of
                b :: [] ->
                    Just b

                _ ->
                    Nothing

        name : String
        name =
            List.filterMap identity
                [ Festival.Locale.localize model.locale a.name
                , List.map (\( _, x ) -> Festival.Locale.localize model.locale x.name) films
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

        alphaValue : Float
        alphaValue =
            if Festival.Model.Utils.eventIsRelevant model ( Id.toAny id, Festival.Data.Screening_ a ) then
                1

            else
                0.5
    in
    column [ width fill, spacing 4, Festival.ElementId.toId (Festival.ElementId.Event (Id.toAny id)), alpha alphaValue ]
        (paragraph theme
            [ fontSemiBold ]
            [ textEllipsis [] name
            ]
            :: paragraph theme
                [ spacing 2, fontSize 14, fontColor style.fore50 ]
                (List.intersperse (text " – ")
                    (List.filterMap identity
                        [ Just (text (Festival.Utils.Translation.date Festival.Data.timeZone a.time))
                        , Just (text (Festival.Utils.Translation.time Festival.Data.timeZone a.time ++ "–" ++ Festival.Utils.Translation.time Festival.Data.timeZone endTime))
                        , Maybe.map
                            (\x ->
                                newTabLink theme
                                    []
                                    { label = text (Festival.Locale.localize model.locale x.name)
                                    , url = Festival.GeoCoordinates.mapyCzLink x.coordinates
                                    }
                            )
                            place
                        , Just (text (Id.toString id))
                        , Just (text (String.join ", " (List.map (\x -> Festival.Locale.localize model.locale x.name) categories)))
                        , Maybe.map
                            (\( x, _ ) ->
                                newTabLink theme
                                    []
                                    { label = text "Info"
                                    , url = Festival.Data.filmLink model.locale x
                                    }
                            )
                            onlyOneFilm
                        , Maybe.map
                            (\( _, x ) ->
                                newTabLink theme
                                    []
                                    { label = text "CSFD"
                                    , url = Festival.Data.csfdLink x
                                    }
                            )
                            onlyOneFilm
                        , Maybe.map
                            (\( _, x ) ->
                                newTabLink theme
                                    []
                                    { label = text "IMDb"
                                    , url = Festival.Data.imdbLink x
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
                                                            { description = Festival.Locale.localize model.locale x.name
                                                            , src = x2
                                                            }
                                                    )
                                                    x.images
                                                )
                                            , text (Festival.Utils.Html.stripTags (Festival.Locale.localize model.locale x.description))
                                            ]

                                        Nothing ->
                                            [ case List.head x.images of
                                                Just x2 ->
                                                    image [ alignRight, width (px 128), height (px 72) ]
                                                        { description = Festival.Locale.localize model.locale x.name
                                                        , src = x2
                                                        }

                                                Nothing ->
                                                    none
                                            , newTabLink theme
                                                []
                                                { label = text (Festival.Locale.localize model.locale x.name)
                                                , url = Festival.Data.csfdLink x
                                                }
                                            , text " "
                                            , text (Festival.Utils.Html.stripTags (Festival.Locale.localize model.locale x.description))
                                            ]
                                    )
                                , paragraph theme
                                    [ spacing 2, fontSize 10, fontColor style.fore50 ]
                                    [ text (String.fromInt x.year)
                                    , text " | "
                                    , text (Festival.Locale.localize model.locale x.country)
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
