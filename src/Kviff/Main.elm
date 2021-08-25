module Kviff.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import Kviff.Program as Program
import Kviff.Router as Router
import Kviff.Translation as Translation
import Kviff.Ui.Base exposing (..)
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
    , program : Program.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( router, routerCmd ) =
            Router.init url key

        ( program, programCmd ) =
            Program.init
    in
    ( { router = router
      , program = program
      }
    , Cmd.batch
        [ routerCmd
            |> Cmd.map RouterMsg
        , programCmd
            |> Cmd.map ProgramMsg
        ]
    )



--


type Msg
    = RouterMsg Router.Msg
    | ProgramMsg Program.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)

        ProgramMsg a ->
            Program.update a model.program
                |> Tuple.mapBoth (\v -> { model | program = v }) (Cmd.map ProgramMsg)



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--


view : Model -> Browser.Document Msg
view model =
    { title = Translation.title
    , body =
        [ adaptiveScale
        , layout [] (Program.view model.program |> map ProgramMsg)
        ]
    }
