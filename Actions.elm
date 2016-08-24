module Actions exposing (Action(..))
import Time exposing (Time)

type Action
  = Tick Time
  | Jump
  | Restart
  | NoOp