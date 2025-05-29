port module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Tick Float GetKeyState
      | Send


-- PORTS 

port sendSpeech : String -> Cmd msg


-- MODEL

type alias Model = {text : String}


-- init : () -> (Model, Cmd Msg)
-- init _ = ({text = "Hello from Elm"}, Cmd.none)
init : Model
init = {text = "Hello from Elm"}



-- UPDATE


-- type Msg
--   = Send


-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--   case msg of
--     Send -> (model, sendSpeech model.text)
--     Tick _ ( keys, _, _ ) -> (model, Cmd.none)

update : Msg -> Model -> Model
update msg model =
  case msg of
    Send -> model
    Tick _ ( keys, _, _ ) -> model



-- SUBSCRIPTIONS

-- Subscribe to the `receiveAudio` port to hear about responses coming in from JS.
-- subscriptions : Model -> Sub Msg
-- subscriptions _ = receiveAudio Receive



-- VIEW

view : Model -> Collage userMsg
view model =
  collage 500 500
        [ line ( 0, 0 ) ( 250, 0 )
            |> outlined (solid 1) green
        ]


-- MAIN

main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        , title = "Title"
        }