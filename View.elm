module View exposing (scene)

import Collage exposing (..)
import Element exposing (..)
import Html exposing (div, Html, text, button, img)
import Html.Attributes exposing (style, src, class)
import Html.Events exposing (onClick)

import Actions exposing (..)
import Model exposing (..)
import Update exposing (..)

scene: Model -> Html Action
scene model =
  div
    [ style
      [
        ("width", "480px"),
        ("height", "640px"),
        ("margin", "0 auto"),
        ("position", "relative")
      ]
    ]
    [ renderGame model
    , renderGround model
    , renderScoreBoard model.scoreBoard model.state
    ]

renderGame : Model -> Html Action
renderGame { x, y, vy, gx, w, h, clock, state, pipes, scoreBoard, birdPad } =
  let
    pos = ( x, y )

    background =
      fittedImage w h "assets/background.png"
        |> Collage.toForm

    bird =
      croppedImage (round birdPad, 0) 58 40 "assets/bird.png"
        |> Collage.toForm
        |> Collage.move pos

    obstacles =
      List.concat ( List.map (\(f, s) -> [renderPipe f, renderPipe s]) pipes )

    elements = [ background ] ++ obstacles ++ [ bird ]
  in
    Collage.collage w h elements
      |> Element.toHtml

renderGround: Model -> Html Action
renderGround model =
  div
    [ style
      [ ("background-image", "url('assets/ground.png')")
      , ("background-position", toString model.gx ++ "px 0px")
      , ("width", "100%")
      , ("height", "70px")
      ]
    ]
    []

renderScoreBoard: Scoreboard -> State -> Html Action
renderScoreBoard scoreBoard state =
  div
    [ style
      [ ("position", "absolute")
      , ("left", "180px")
      , ("top", "240px")
      , ("opacity", toString scoreBoard.opacity)
      ]
    ]
    [ img
        [
          onClick Restart,
          src "assets/restart.png"
        ]
      []
    ,
      div
        [ style
          [ ("font-size","40px")
          , ("color","white")
          , ("text-align", "center")
          , ("text-shadow", "0px 0px 1px rgba(0, 0, 0, 1)")
          ]
        ]
        [ Html.text (toString scoreBoard.score) ]
    ]

renderPipe : Pipe -> Form
renderPipe { w, h, angle, x, y } =
  croppedImage (0, 0) (round w) ( round h ) "assets/pipe.png"
    |> Collage.toForm
    |> Collage.rotate (degrees angle)
    |> Collage.move ( x, y )