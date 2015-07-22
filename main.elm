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
  , selection : Maybe String
  }

type alias Friend = {
  name : String
  , photo : String
  }

init : Model
init = Model "d" (Just "Dean") -- "" Nothing
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
      handleSelect f = onClick address (Select f.name)
      results =
        let filtered =
          case model.query of
            "" -> []
            s ->
              List.map (viewFriend handleSelect) <| List.filter (matches s) friends
        in
        ul [] filtered
      selection  =
        case model.selection of
          Nothing -> div [] []
          Just x -> text x
  in
  div [] [qInput, results, selection]

viewFriend handleClick f = li [handleClick f] [text f.name, text f.photo]
matches s f =
  String.contains (String.toLower s) (String.toLower f.name)

