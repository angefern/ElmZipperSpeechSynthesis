port module Main exposing (..)

import Url exposing (..)
import Browser.Navigation exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Tick Float GetKeyState
      | Send
      | NoOp


-- PORTS 

port sendSpeech : String -> Cmd msg


-- MODEL

type alias Model = {text : String}


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ = ({text = "Hello from Elm"}, Cmd.none)


-- UPDATE


-- type Msg
--   = Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Send -> (model, sendSpeech model.text)
    Tick _ ( keys, _, _ ) -> (model, Cmd.none)
    NoOp -> (model, Cmd.none)



-- SUBSCRIPTIONS

-- Subscribe to the `receiveAudio` port to hear about responses coming in from JS.
-- subscriptions : Model -> Sub Msg
-- subscriptions _ = receiveAudio Receive
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> { title : String, body : Collage Msg }
view model =
  { title = "App",
    body = collage 500 500
            [ line ( 0, 0 ) ( 250, 0 )
                |> outlined (solid 1) green
            ]
  }


-- MAIN
main : AppWithTick () Model Msg
main =
    appWithTick Tick
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }