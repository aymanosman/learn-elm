import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Signal exposing ((<~),(~))


type alias Game =
  { dir : Dir
  , snake : Snake
  }

type alias Snake =
  { head : Pos
  , body : List Pos
  }

type alias Input =
  { space : Bool
  , wasd : { x : Int, y : Int }
  , delta : Time
  }

type alias Pos = { x : Int, y : Int }

type Dir = Up | Down | Left | Right

main : Signal Element
main =
  view2 <~ Window.dimensions ~ input ~ game

game : Signal Game
game =
  Signal.foldp update init input

delta : Signal Float
delta =
  Signal.map inSeconds (fps 10)

input : Signal Input
input =
  Signal.sampleOn delta <|
  Input <~ Keyboard.space ~ Keyboard.wasd ~ delta

init : Game
init =
  { dir = Right
  , snake = {head = Pos 2 0, body = [Pos 1 0, Pos 0 0]}
  }

update : Input -> Game -> Game
update {wasd} m =
  let
    newDir dir {x,y} =
     if
     | y == 1 -> Up
     | y == -1 -> Down
     | x == 1 -> Right
     | x == -1 -> Left
     | otherwise -> dir
  in
  { m |
    dir <- newDir m.dir wasd
  , snake <- moveSnake m.dir m.snake
  }

moveSnake : Dir -> Snake -> Snake
moveSnake dir {head, body} =
  let
    newHead =
      case dir of
        Up -> -- over .y (+1)
          {head | y <- head.y + 1}
        Down ->
          {head | y <- head.y - 1}
        Left ->
          {head | x <- head.x - 1}
        Right ->
          {head | x <- head.x + 1}

    butLast xs = List.take (List.length xs - 1) xs
  in
  Snake newHead (head::butLast body)

view2 : (Int, Int) -> Input -> Game -> Element
view2 (w,h) input game =
  let
    someState =
      { input = input
      , game =
          { dir = game.dir
          , snakeHead = game.snake.head
          }
      }
  in
  flow down
  [ show someState
  , view (w, h) game
  ]

view : (Int, Int) -> Game -> Element
view (w, h) game =
  collage w h <|
  [ rect (toFloat w/2) (toFloat h/2)
    |> filled grey
  , drawSnake game.snake
  ]

drawSnake : Snake -> Form
drawSnake {head, body} =
  group <| (drawPoint head)::List.map drawPoint body

drawPoint : Pos -> Form
drawPoint {x,y} =
  let
    (a, b) = (toFloat (x*10), toFloat (y*10))
  in
  rect 10 10 |> filled greenw |> move (a, b)

