module Kviff.Program exposing (..)

import Http
import Kviff.Api as Api
import Kviff.Translation as Translation
import Kviff.Ui.Base exposing (..)
import Task exposing (Task)


type alias Model =
    { locale : Api.Locale
    , program : Result Error (List Api.Event)
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { locale = Api.English
      , program = Err Loading
      }
    , Api.getProgram
        |> Task.attempt GotProgram
    )



--


type Msg
    = ChangeLocale Api.Locale
    | GotProgram (Result Http.Error (List Api.Event))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocale b ->
            ( { model | locale = b }
            , Cmd.none
            )

        GotProgram b ->
            ( { model | program = b |> Result.mapError HttpError }
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
    textColumn [ padding 16, spacing 16, width (fill |> maximum (320 * 2)), centerX ]
        [ row []
            [ h1 []
                [ text Translation.title
                ]
            , localeChooser
            ]
        , case model.program of
            Ok b ->
                viewProgram model b

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


viewProgram : Model -> List Api.Event -> Element Msg
viewProgram model a =
    textColumn [ spacing 16 ]
        (List.map (viewEvent model) a)


viewEvent : Model -> Api.Event -> Element Msg
viewEvent model a =
    textColumn [ spacing 4 ]
        [ textColumn [ spacing 2 ]
            [ h2 []
                [ text (Api.localize model.locale a.name)
                ]
            , p [ fontSize 14, fontColor grey4 ]
                [ text
                    ([ a.type_ |> Translation.type_ |> Just
                     , a.place.name |> Api.localize model.locale |> Just
                     , a.timeStart
                     , a.timeEnd
                     ]
                        |> List.filterMap identity
                        |> String.join " – "
                    )
                ]
            ]
        , p [ fontSize 14, fontColor grey4 ]
            [ text (Api.localize model.locale a.description)
            ]
        ]
