import Dict exposing (Dict)
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
    , update = withDebug << withLast <| update
  }

-- Model

type alias Model = {
  query : String
  , selected : Maybe String
  , highlighted : Int
  , lastAction : Action
  }

init : Model
init = Model "d" (Just "Dean") 1 NoOp -- "" Nothing

type alias Friend = {
  name : String
  , photo : String
  }

f : String -> String -> Friend
f a b = Friend a b

type Action = NoOp
  | Query String
  | Select Id -- change to Int and pick from Dict
  | EnterSelect
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
      {model | selected <- Just n}
    EnterSelect ->
        let
            tagged = mkTagged model.query
        in
        case Dict.get model.highlighted tagged of
            Nothing -> model
            Just f -> {model | selected <- Just f.name}
    Next ->
      {model | highlighted <- model.highlighted + 1}
    Prev ->
      {model | highlighted <- model.highlighted - 1}

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
          , onKeyDown address (\k -> translate2 k)
          , value model.query
          ] []
      handleSelect f = onClick address (Select f.name)
      results =
          let
            tagged = mkTagged model.query
            rendered =
                Dict.values <|
                Dict.map
                (\k v -> viewFriend handleSelect model.highlighted (k,v))
                tagged

        in
        ul [] rendered
      selection  =
        case model.selected of
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

matches s f =
  String.contains (String.toLower s) (String.toLower f.name)

translate2 k =
  case k of
    38 -> Prev
    40 -> Next
    13 -> EnterSelect
    _ -> NoOp

friends : List Friend
friends = [f "Ayman" "", f "Jesus" "", f "Dave" "", f "DJ" "", f "Dean" ""]

mkTagged : String -> Dict Int Friend
mkTagged q =
    let filtered : List Friend
        filtered =
            case q of
                "" -> []
                s -> List.filter (matches s) friends
    in
    Dict.fromList <| List.map2 (,) [1..List.length filtered] filtered

