module Kviff.Program exposing (..)

import Browser.Dom as Dom
import Html
import Http
import Kviff.Api as Api
import Kviff.Translation as Translation
import Kviff.Ui.Base exposing (..)
import Task exposing (Task)
import Time
import Url exposing (Url)
import Utils.Html


type alias Model =
    { locale : Api.Locale
    , time : Maybe Time.Posix
    , data : Result Error (List Api.Event)
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { locale = Api.Czech
      , time = Nothing
      , data = Err Loading
      }
    , Cmd.batch
        [ Time.now
            |> Task.perform GotTime
        , Api.getData
            |> Task.attempt GotData
        ]
    )



--


type Msg
    = ChangeLocale Api.Locale
    | GotTime Time.Posix
    | GotData (Result Http.Error Api.Data)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocale b ->
            ( { model | locale = b }
            , Cmd.none
            )

        GotTime b ->
            ( { model | time = Just b }
            , Cmd.none
            )

        GotData b ->
            ( { model | data = b |> Result.map normalizeData |> Result.mapError HttpError }
            , Cmd.none
            )


scrollToUpcomingEvent : Model -> Task Dom.Error ()
scrollToUpcomingEvent model =
    let
        upcomingEvents : Result Error (List ( Int, Api.Event ))
        upcomingEvents =
            model.data
                |> Result.map
                    (\v1 ->
                        v1
                            |> List.indexedMap Tuple.pair
                            |> List.filter
                                (\( _, v2 ) ->
                                    Maybe.map2
                                        (\startTime now ->
                                            Time.posixToMillis startTime > Time.posixToMillis now
                                        )
                                        v2.startTime
                                        model.time
                                        |> Maybe.withDefault False
                                )
                    )
    in
    case upcomingEvents |> Result.toMaybe |> Maybe.andThen List.head of
        Just ( id, _ ) ->
            Dom.getElement (eventId id)
                |> Task.andThen
                    (\v ->
                        Dom.setViewport v.element.x v.element.y
                    )

        Nothing ->
            Task.succeed ()



--


view : Model -> Element Msg
view model =
    let
        localeChooser : Element Msg
        localeChooser =
            case model.locale of
                Api.English ->
                    link_ [ fontSize 14 ]
                        { label = text "CZ"
                        , onPress = Just (ChangeLocale Api.Czech)
                        }

                Api.Czech ->
                    link_ [ fontSize 14 ]
                        { label = text "EN"
                        , onPress = Just (ChangeLocale Api.English)
                        }
    in
    textColumn [ spacing 32, padding 16, width (fill |> maximum (320 * 2)), centerX ]
        [ row [ spacing 8 ]
            [ h1 []
                [ text Translation.title
                ]
            , localeChooser
            ]
        , case model.data of
            Ok b ->
                viewEvents model b

            Err b ->
                viewError b
        , text ""
        , p [ fontCenter, fontSize 14, fontColor grey5 ]
            [ text Translation.footer
            ]
        ]


viewError : Error -> Element msg
viewError b =
    p [ fontSize 14, fontColor grey4 ]
        [ case b of
            Loading ->
                text "Loading..."

            HttpError c ->
                case c of
                    Http.Timeout ->
                        text "There is a network error. Try reload."

                    Http.NetworkError ->
                        text "There is a network error. Try reload."

                    _ ->
                        text "Sorry, but application is not available."
        ]


viewEvents : Model -> List Api.Event -> Element Msg
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
    textColumn [ spacing 20 ]
        (imgContain :: List.indexedMap (viewEvent model) a)


viewEvent : Model -> Int -> Api.Event -> Element Msg
viewEvent model index a =
    textColumn [ spacing 4, id (eventId index) ]
        [ textColumn [ spacing 2 ]
            [ h2 [ fontSemiBold ]
                [ text (Api.localize model.locale a.name)
                ]
            , p [ fontSize 14, fontColor grey4 ]
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



--


eventId : Int -> String
eventId a =
    "e-" ++ String.fromInt a


normalizeData : Api.Data -> List Api.Event
normalizeData a =
    a
        |> Api.dataToEvents
        |> List.filter
            (\v ->
                v.type_ /= Api.Restaurant
            )
        |> List.sortBy
            (\v ->
                v.startTime
                    |> Maybe.map Time.posixToMillis
                    |> Maybe.withDefault 0
            )
