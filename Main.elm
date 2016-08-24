module Main exposing (..)

import Time exposing (Time, every, second, now)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Html.App exposing (program)

import Actions exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)

subs : Model -> Sub Action
subs model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs (\code -> key code model)
        ]

key : KeyCode -> Model -> Action
key keycode model =
  case keycode of
      32 -> if model.state /= GameOver then
              Jump
            else if model.state == GameOver && model.scoreBoard.opacity == 1 then
              Restart
            else
              NoOp
      _ -> NoOp

main =
    program
        { init = ( Model.initial, Cmd.none )
        , update = (\action model -> ( update action model, Cmd.none ))
        , subscriptions = subs
        , view = scene
        }
