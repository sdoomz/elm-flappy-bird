module Update exposing (update)

import Time exposing (Time)
import Random

import Actions exposing (..)
import Model exposing (..)

update : Action -> Model -> Model
update action model =
    case action of
        Tick dt ->
          model
            |> removeObstacles
            |> spawnObstacles
            |> animateVertical dt
            |> animateHorizontal
            |> checkCollisions
            |> showScoreBoard

        Jump ->
          if model.state == Ready then
            { model | state = Play, direction = None, vy = -8 }
          else if model.state == Play then
            { model | vy = -8 }
          else
            model
        Restart ->
          Model.initial
        NoOp ->
           model

animateVertical : Time -> Model -> Model
animateVertical dt model =
  let
    clock =
      model.clock + dt

    yMax =
      toFloat model.h / 2 + 100

    direction' =
      if model.direction == Top && model.y >= 10 then
        Bottom
      else if model.direction == Bottom && model.y <= -10 then
        Top
      else
        model.direction

    vy' =
      if model.vy < 10 then
        model.vy + 0.5
      else
        model.vy

    y' =
      if model.y <= 10 && model.direction == Top then
        model.y + 1
      else if model.y >= -10 && model.direction == Bottom then
        model.y - 1
      else if model.y > yMax then
        yMax
      else if model.y > -275 then
        model.y - vy'
      else
        model.y
  in
    { model | clock = clock, y = y', vy = vy', direction = direction' }

animateHorizontal : Model -> Model
animateHorizontal model =
  if model.state /= GameOver then
    let
       gx' =
         if model.gx > -36 then
           model.gx - 3
         else
           0
       pipes =
           List.map ( \(top, bottom) -> ({ top | x = top.x - 3}, { bottom | x = top.x - 3}) ) model.pipes
    in
      { model | gx = gx', pipes = pipes }
  else
    model

hasRedundantPipes: Model -> Bool
hasRedundantPipes model =
  let
     pipes = List.head model.pipes
  in
    case pipes of
      Just p ->
        let
          first = fst p
        in
          first.x + first.w / 2 < toFloat model.w / -2
      Nothing -> False

hasPastPipes: Maybe ( Pipe, Pipe )-> Bool
hasPastPipes pipe =
  case pipe of
    Just p ->
      let
        first = fst p
      in
        first.x + first.w / 2 < -50
    Nothing -> True

removeObstacles : Model -> Model
removeObstacles model =
  if model.state == Play && hasRedundantPipes model then
    { model | pipes = List.drop 1 model.pipes }
      |> incrementScore model.scoreBoard
  else
    model

spawnObstacles: Model -> Model
spawnObstacles model =
  if model.state == Play && hasPastPipes (model.pipes |> List.reverse |> List.head) then
    let
      seed  =
        Random.initialSeed (round model.clock)
      random =
        Random.step (Random.float -100 100) seed |> fst
      height1 =
        200 - random + 100
      y1 =
        ( toFloat model.h - height1 ) / 2 + 100

      height2 =
        200 + random
      y2 =
        (toFloat -model.h + height2 ) / 2

      top =
        Pipe 285 y1 180 90 height1
      bottom =
        Pipe 285 y2 0 90 height2

      pipes =
        List.append model.pipes [(top, bottom)]
    in
     { model | pipes = pipes }
  else
    model

calculateFinalScore : Model -> Model
calculateFinalScore model =
  if hasPastPipes (model.pipes |> List.head) then
    incrementScore model.scoreBoard model
  else
    model

incrementScore : Scoreboard -> Model -> Model
incrementScore scoreBoard model =
  { model | scoreBoard = { scoreBoard | score = scoreBoard.score + 1 } }

checkCollisions : Model -> Model
checkCollisions model =
  if model.state /= GameOver && ( model.y <= -275 || (model.pipes |> List.map (\(f, s) -> [f, s]) |> List.concat |> List.any (\p -> hasCollision p model )) ) then
    let
       model' = calculateFinalScore model
    in
      { model' | state = GameOver }
  else
    model

hasCollision : Pipe -> Model -> Bool
hasCollision pipe { x, y, w, h } =
  if
    (x - 29 < pipe.x + pipe.w / 2) &&
    (x + 29 > pipe.x - pipe.w / 2) &&
    (y - 20 < pipe.y + pipe.h / 2) &&
    (y + 20 > pipe.y - pipe.h / 2) then
    True
  else
    False

showScoreBoard : Model -> Model
showScoreBoard model =
    if model.state /= GameOver then
      model
    else
      let
        scoreBoard' = model.scoreBoard
        opacity =
          if scoreBoard'.opacity < 1 then
            scoreBoard'.opacity + 0.1
          else
            1
      in
        { model | scoreBoard = { scoreBoard' | opacity = opacity } }

