import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug exposing (log)
import StartApp
import Json.Decode as Json

main =
  StartApp.start {
    model = init
    , view = view
    , update = withDebug << withLast <| update
  }

-- Model

type alias Model = {
  query : String
  , selected : Maybe Friend
  , highlighted : Int
  , lastAction : Action
  }

init : Model
init = {
  query = ""
  , selected = Nothing
  , highlighted = 1
  , lastAction = NoOp
  }

type alias Friend = {
  name : String
  , photo : String
  }

type Action = NoOp
  | Query String
  | ClickSelect Friend
  | EnterSelect
  | Next
  | Prev

type alias Id = String

-- Update

update : Action -> Model -> Model
update action model =
  let select f m =
        {m | query <- f.name, selected <- Just f}
  in
  case action of
    NoOp -> model
    Query t ->
      {model | highlighted <- 1, query <- t, selected <- Nothing}
    ClickSelect f ->
      select f model
    EnterSelect ->
        let tagged = mkTagged model.query
        in case Dict.get model.highlighted tagged of
            Nothing -> model
            Just f -> select f model
    Next ->
      let numFriends = List.length <| Dict.toList (mkTagged model.query)
      in
      if model.highlighted == numFriends
      then model
      else {model | highlighted <- model.highlighted + 1}
    Prev ->
      if model.highlighted == 1
      then model
      else {model | highlighted <- model.highlighted - 1}

withDebug update action model = Debug.watch "State" (update action model)

withLast update action model =
    let m2 = update action model
    in
    {m2 | lastAction <- action}

-- View

view : Signal.Address Action -> Model -> Html
view addr model =
  let options = {preventDefault = True, stopPropagation = False}
      handleKeyDown f =
        onWithOptions "keydown" options (Json.customDecoder keyCode isKey) f
      isKey k =
          if k == 38 || k == 40 || k == 13
          then Ok k
          else Err "not interested in this key"

      qInput =
        input
          [ on "input" targetValue (Signal.message addr << Query)
          , handleKeyDown (\k -> Signal.message addr <|
                case k of
                    38 -> Prev
                    40 -> Next
                    13 -> EnterSelect)
          , value model.query
          , autofocus True
          ] []
      handleSelect f = onClick addr (ClickSelect f)
      results =
        let
        tagged = mkTagged model.query
        rendered =
            Dict.values <|
            Dict.map
            (\k v -> viewFriend handleSelect (model.highlighted == k) v)
            tagged

        in
        ul [] rendered
      selection  =
        case model.selected of
          Nothing -> div [] []
          Just f -> text f.name
  in
  case model.selected of
    Nothing -> div [] [qInput, results]
    Just f -> div [] [qInput, selection]

viewFriend : (Friend -> Attribute) -> Bool -> Friend -> Html
viewFriend handleSelect hl f =
    let attrs : List Attribute
        attrs = [handleSelect f]
        hlStyle = style [("background-color", "salmon")]
        attrs2 : List Attribute
        attrs2 = if hl then hlStyle::attrs else attrs
    in
    li attrs2 [text f.name, text f.photo]

matches s f =
  String.contains (String.toLower s) (String.toLower f.name)

friends : List Friend
friends =
  let f a b = Friend a b
  in
  [f "Ayman" "", f "Jesus" "", f "Dave" "", f "DJ" "", f "Dean" ""]

mkTagged : String -> Dict Int Friend
mkTagged q =
    let filtered : List Friend
        filtered =
            case q of
                "" -> []
                s -> List.filter (matches s) friends
    in
    Dict.fromList <| List.map2 (,) [1..List.length filtered] filtered
