import Graphics.Element exposing (..)
import Mouse
import Time exposing (..)

main : Signal Element
main = Signal.map view input

delays = [0, 0.5, 1]

type alias Input = List (Int, Int)

seq : List (Signal a) -> Signal (List a)
seq ss =
  case ss of
    [] -> Signal.constant []
    x::xs -> Signal.map2 (::) x (seq xs)

input : Signal Input
input =
  let
    f = (\d -> delay (d*1000) Mouse.position)
  in
  List.map f delays
  |> seq

view : Input -> Element
view i =
  show i
