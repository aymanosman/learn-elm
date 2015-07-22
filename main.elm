import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug
import StartApp

main =
  StartApp.start {
    model = init
    , view = view
    , update = update
  }

-- Model

type alias Model = {
  query : String
  , friends : List Friend
  , selection : Maybe String
  }

type alias Friend = {
  name : String
  , photo : String
  }

init : Model
init = Model "" friends Nothing
friends =
  [f "Ayman" "", f "Jesus" "", f "Dave" "", f "DJ" "", f "Dean" ""]

f : String -> String -> Friend
f a b = Friend a b

type Action = Query String | Select Id

type alias Id = String

-- Update

update action model =
  let x =
      case action of
        Query t ->
          {model | query <- t}
        Select n ->
          {model | selection <- Just n}
  in
  x |> Debug.watch "State"

-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let qInput =
        input
          [on "input" targetValue (Signal.message address << Query)
          , value model.query
          ] []
      results =
        let filtered =
          case model.query of
            "" -> []
            s -> filterFriends address s model.friends
        in
        ul [] filtered
      selection  =
        case model.selection of
          Nothing -> div [] []
          Just x -> text x
  in
  div [] [qInput, results, selection]

viewFriend address f = li [onClick address (Select f.name)] [text f.name, text f.photo]
filterFriends address s fs =
  List.map (viewFriend address)
    <| List.filter (\f -> String.contains (String.toLower s) (String.toLower f.name)) fs

