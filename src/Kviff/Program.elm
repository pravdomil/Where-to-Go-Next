module Kviff.Program exposing (..)

import Http
import Kviff.Api as Api
import Kviff.Translation as Translation
import Kviff.Ui.Base exposing (..)
import Task exposing (Task)
import Time
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
    textColumn [ spacing 20 ]
        (List.map (viewEvent model) a)


viewEvent : Model -> Api.Event -> Element Msg
viewEvent model a =
    textColumn [ spacing 4 ]
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
                 , a.timeStart
                    |> Maybe.map (\v -> text (Translation.time v))
                 , Maybe.map2
                    (\start end ->
                        text (Translation.duration (Time.posixToMillis end - Time.posixToMillis start))
                    )
                    a.timeStart
                    a.timeEnd
                 ]
                    |> List.filterMap identity
                    |> List.intersperse (text " – ")
                )
            ]
        , case a.image of
            Just b ->
                image [ width (px 128), alignRight ]
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
                v.timeStart
                    |> Maybe.map Time.posixToMillis
                    |> Maybe.withDefault 0
            )
