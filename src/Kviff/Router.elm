module Kviff.Router exposing (..)

import Browser
import Browser.Navigation as Navigation
import Task
import Url exposing (Url)


type alias Model =
    { key : Navigation.Key
    }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url key =
    ( { key = key
      }
    , Task.succeed () |> Task.perform (\_ -> UrlChanged url)
    )



--


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested a ->
            case a of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged a ->
            ( model
            , Cmd.none
            )
