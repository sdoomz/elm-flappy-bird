module Model exposing (Direction(..), State(..), Pipe, Model, Scoreboard, initial)

import Time exposing (Time, second)

type Direction = Top | Bottom | None
type State = Ready | Play | GameOver

type alias Scoreboard =
  { x: Float
  , y: Float
  , w: Float
  , h: Float
  , opacity: Float
  , score: Int
  }
type alias Pipe =
  { x: Float,
    y: Float,
    angle: Float,
    w: Float,
    h: Float
  }

type alias Model =
  { x : Float,
    y : Float,
    vy: Float,
    gx: Float,
    w : Int,
    h : Int,
    scoreBoard: Scoreboard,
    clock : Time,
    state : State,
    pipes : List ( Pipe, Pipe ),
    direction: Direction,
    birdPad: Float
  }

initial : Model
initial =
  { x = -50
  , y = 0
  , vy = 0
  , gx = 0
  , w = 480
  , h = 570
  , scoreBoard = (Scoreboard 0 0 100 100 0 0)
  , clock = 0
  , state = Ready
  , pipes = []
  , direction = Bottom
  , birdPad = 0
  }