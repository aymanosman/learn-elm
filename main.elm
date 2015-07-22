import Dict exposing (..)
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
    , update = withDebug update
  }

-- Model

type alias Model = {
  query : String
  , selection : Maybe String
  , hl : Int
  }

init : Model
init = Model "d" (Just "Dean") 1 -- "" Nothing

type alias Friend = {
  name : String
  , photo : String
  }

friends : Dict Int Friend
friends =
  Dict.fromList <|
      List.map2 (,) [1..100] [f "Ayman" "", f "Jesus" "", f "Dave" "", f "DJ" "", f "Dean" ""]

f : String -> String -> Friend
f a b = Friend a b

type Action = NoOp | Query String | Select Id | HightlightNext

type alias Id = String

-- Update

update action model =
  case action of
    NoOp -> model
    Query t ->
      {model | query <- t}
    Select n ->
      {model | selection <- Just n}
    HightlightNext ->
      {model | hl <- model.hl + 1}

withDebug update action model = (update action model) |> Debug.watch "State"

-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let qInput =
        input
          [on "input" targetValue (Signal.message address << Query)
          , onDownArrow address HightlightNext
          -- , onKeyPress address (handleKeyPress (List.head friends |> .name))
          , value model.query
          ] []
      handleSelect f = onClick address (Select f.name)
      results =
        let filtered =
          case model.query of
            "" -> []
            s ->
              List.map (viewFriend handleSelect) <| List.filter (matches s) <| Dict.values friends
        in
        ul [] filtered
      selection  =
        case model.selection of
          Nothing -> div [] []
          Just x -> text x
  in
  div [] [qInput, results, selection]

viewFriend handleSelect f = li [handleSelect f] [text f.name, text f.photo]
matches s f =
  String.contains (String.toLower s) (String.toLower f.name)

handleKeyPress n k =
    case k of
        39 -> Select n

onDownArrow addr a =
    onKeyDown addr (\k ->
        case k of
            40 -> HightlightNext
            -- 13 Enter
            _ -> NoOp)
