port module Main exposing (..)

import Url exposing (..)
import Browser.Navigation exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Tick Float GetKeyState
      | NoOp


-- PORTS 

port sendSpeech : String -> Cmd msg


-- MODEL

type alias Model = {time : Float
                   , qZip : SentenceZipper }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg ) --?
init _ _ _ = ({time = 0.0, qZip = SZip [] "The" ["bunny", "is", "red"]}, sendSpeech "The")


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- Send -> (model, sendSpeech model.text)
    Tick t ( getKeyState, _, _ ) -> let 
                                      (before, current, after) = case model.qZip of
                                                            SZip b c a -> (b, c, a)
                                    in (if getKeyState RightArrow == JustDown then
                                          case after of
                                              [] -> ({ model | time = t 
                                                      , qZip = sRight model.qZip 
                                                      }, sendSpeech current)   
                                              x::_ -> ({ model | time = t 
                                                      , qZip = sRight model.qZip 
                                                      }, sendSpeech x)   
                                        else if getKeyState LeftArrow == JustDown then
                                            case before of
                                                [] -> ({ model | time = t 
                                                        , qZip = sLeft model.qZip 
                                                        }, sendSpeech current)
                                                x::_ -> ({ model | time = t 
                                                          , qZip = sLeft model.qZip 
                                                          }, sendSpeech x)
                                        else if getKeyState Backspace == JustDown then
                                            case before of
                                                [] -> ({ model | time = t 
                                                        , qZip = sBackspace model.qZip 
                                                        }, sendSpeech current)
                                                _ -> ({ model | time = t 
                                                          , qZip = sBackspace model.qZip 
                                                          }, sendSpeech ("deleted "++current))
                                        else if getKeyState Delete == JustDown then
                                            case after of
                                                [] -> ({ model | time = t 
                                                        , qZip = sDelete model.qZip 
                                                        }, sendSpeech current)
                                                _ -> ({ model | time = t 
                                                          , qZip = sDelete model.qZip 
                                                          }, sendSpeech ("deleted "++current))
                                        else
                                          ({model | time = t, qZip = SZip before current after}, Cmd.none)
                                    )
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
            [ 
              viewZipper model.qZip
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


---- Zipper Logic

type SentenceZipper = SZip (List String) String (List String) -- before, current, after
-- note that before is a stack (most recent previous element at head)
-- but after is a regular list (next elements in reading order)

sLeft : SentenceZipper -> SentenceZipper
sLeft (SZip before current after) = case before of
                                      [] -> SZip before current after
                                      a::rest -> SZip rest a (current::after)

sRight : SentenceZipper -> SentenceZipper
sRight  (SZip before current after) = case after of
                                         [] -> SZip before current after
                                         a::rest -> SZip (current::before) a rest

-- maybe should previous next word instead of current word... clarify desired behaviour
sBackspace : SentenceZipper -> SentenceZipper
sBackspace (SZip before current after) = case before of 
                                        [] -> SZip before current after -- nothing to backspace
                                        a::rest -> SZip rest a after -- delete current, move focus back to previous word 

-- maybe should delete next word instead of current word... clarify desired behaviour
sDelete : SentenceZipper -> SentenceZipper
sDelete (SZip before current after) = case after of 
                                        [] -> SZip before current after -- nothing to delete
                                        a::rest -> SZip before a rest -- delete current, move focus forward to next word 


viewWord : String -> Shape Msg
viewWord word = 
    text word |> fixedwidth |> filled black

type alias PositionedWord = (String, Float)

positionWords : Float -> List String -> List PositionedWord
positionWords start words =
    List.foldl
        (\word (accum, pos) ->
            let
                wordWidth = toFloat (String.length word) * 10 -- adjust multiplier as needed
                newItem = (word, pos)
                newPos = pos + wordWidth
            in
            (newItem :: accum, newPos)
        )
        ([], start)
        words
        |> Tuple.first
        |> List.reverse

viewPositionedWord : PositionedWord -> Shape Msg
viewPositionedWord (word, x) =
    text word
        |> fixedwidth
        |> filled black
        |> move (x, 0)


viewZipper : SentenceZipper -> Shape Msg
viewZipper (SZip before current after) =
    let
        -- Starting x-coordinate
        start = 0

        -- Reverse before to show left-to-right
        beforeWords = List.reverse before

        -- Position words from left of current
        positionedBefore = positionWords start beforeWords

        -- Compute current word's x-position
        offsetBefore = List.foldl (\w acc -> acc + toFloat (String.length w) * 10) 0 beforeWords

        -- Position current word
        currentShape = viewWord current |> move (offsetBefore, 0) |> addOutline (solid 0.5) blue

        -- Position after words
        positionedAfter = positionWords (offsetBefore + toFloat (String.length current) * 10) after

        -- Shapes
        beforeShapes = List.map viewPositionedWord positionedBefore
        afterShapes = List.map viewPositionedWord positionedAfter
    in
        beforeShapes ++ (currentShape::afterShapes)
            |> group |> move (-50,0)