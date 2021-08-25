module Kviff.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import Kviff.Router as Router
import Kviff.Translation as Translation
import Url exposing (Url)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = Router.UrlRequested >> RouterMsg
        , onUrlChange = Router.UrlChanged >> RouterMsg
        }



--


type alias Model =
    { router : Router.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( router, routerCmd ) =
            Router.init url key
    in
    ( { router = router
      }
    , routerCmd
        |> Cmd.map RouterMsg
    )



--


type Msg
    = RouterMsg Router.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--


view : Model -> Browser.Document Msg
view _ =
    { title = Translation.title
    , body =
        [ Html.text Translation.title
        ]
    }
