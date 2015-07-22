import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug exposing (log)
import StartApp

main =
  StartApp.start {
    model = init
    , view = view
    , update = withLast << withDebug <| update
  }

-- Model

type alias Model = {
  query : String
  , selection : Maybe String
  , hl : Int
  , lastAction : Action
  }

init : Model
init = Model "d" (Just "Dean") 1 NoOp -- "" Nothing

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

type Action = NoOp
  | Query String
  | Select Id -- change to Int and pick from Dict
  | Next
  | Prev

type alias Id = String

-- Update

update action model =
  case action of
    NoOp -> model
    Query t ->
      {model | query <- t}
    Select n ->
      {model | selection <- Just n}
    Next ->
      {model | hl <- model.hl + 1}
    Prev ->
      {model | hl <- model.hl - 1}

withDebug update action model = (update action model) |> Debug.watch "State"

withLast update action model =
    let m2 = update action model
    in
    {m2 | lastAction <- action}

-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let qInput =
        input
          [on "input" targetValue (Signal.message address << Query)
          , onArrow Down Next address
          , onArrow Up Prev address
          -- , onKeyPress address (handleKeyPress (List.head friends |> .name))
          , value model.query
          ] []
      handleSelect f = onClick address (Select f.name)
      results =
        let filtered =
          case model.query of
            "" -> []
            s ->
              List.map (viewFriend handleSelect model.hl) <| List.filter (matches s) <| Dict.toList friends
        in
        ul [] filtered
      selection  =
        case model.selection of
          Nothing -> div [] []
          Just x -> text x
  in
  div [] [qInput, results, selection]

-- viewFriend : (Friend -> Attribute) -> Int -> (Int, Friend) -> Html
viewFriend handleSelect hl (i, f) =
    let attrs : List Attribute
        attrs = [handleSelect f]
        hlStyle = style [("background-color", "salmon")]
        attrs2 : List Attribute
        attrs2 = if hl == i then hlStyle::attrs else attrs
    in
    li attrs2 [text f.name, text f.photo]
matches s (_, f) =
  String.contains (String.toLower s) (String.toLower f.name)

handleKeyPress n k =
    case k of
        39 -> Select n

type Dir = Unknown | Up | Down

onArrow : Dir -> Action -> Signal.Address Action ->  Attribute
onArrow dir a addr =
  onKeyDown addr (\k ->
      case translate k of
          Unknown -> log "NoOp" NoOp
          dir -> log (toString a ++ "!!!") a)

translate k =
    case k of
        38 -> log "up" Up
        40 -> log "down" Down
        _ -> log "unkown" Unknown
