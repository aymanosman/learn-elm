import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug exposing (log)
import Json.Decode as Json
import Signal exposing (Address)
import StartApp

main : Signal Html
main =
  StartApp.start {
    model = init
    , view = view
    , update = withDebug << withLast <| update
  }

-- Model

type alias Model = {
  query : String
  , choices : List Friend
  , highlighted : Int
  , selected : Maybe Friend
  , lastAction : Action
  }

init : Model
init = {
  query = ""
  , choices = []
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
  let select f =
        {model | query <- f.name, selected <- Just f, choices <- []}
  in
  case action of
    NoOp -> model
    Query t ->
      {model |
        query <- t
        , choices <- mkChoices t
        , highlighted <- 1
        , selected <- Nothing}
    ClickSelect f ->
      select f
    EnterSelect ->
        let mf = List.head <|
            List.drop (model.highlighted-1) model.choices
        in case mf of
            Nothing -> model
            Just f -> select f
    Next ->
      if model.highlighted == List.length model.choices
      then model
      else {model | highlighted <- model.highlighted + 1}
    Prev ->
      if model.highlighted == 1
      then model
      else {model | highlighted <- model.highlighted - 1}


-- View

view : Address Action -> Model -> Html
view addr model =
  let
    options = {preventDefault = True, stopPropagation = False}
    dec =
      (Json.customDecoder keyCode (\k ->
          if List.member k [13, 38, 40]
          then Ok k
          else Err "not handling that key"))
    queryInput =
      input
        [on "input" targetValue (Signal.message addr << Query)
        , onWithOptions "keydown" options dec (\k ->
            Signal.message addr <|
              case k of
                  38 -> Prev
                  40 -> Next
                  13 -> EnterSelect)
        , value model.query
        , autofocus True
        ] []
  in
  case model.selected of
    Just f -> div [] [queryInput, text f.name]
    Nothing ->
      let
        tagged =
          List.map2 (,) [1..List.length model.choices] model.choices
        handleSelect f = onClick addr (ClickSelect f)
        rendered =
          List.map
          (\(k, v) ->
            viewFriend addr (k == model.highlighted) v)
          tagged
      in
      div [] [queryInput, ul [] rendered]

viewFriend : Address Action -> Bool -> Friend -> Html
viewFriend addr hl f =
    let attrs = [onClick addr (ClickSelect f)]
        hlStyle = style [("background-color", "salmon")]
    in
    li (if hl then hlStyle::attrs else attrs)
    [text f.name, text f.photo]

matches : String -> Friend -> Bool
matches s f =
  String.contains (String.toLower s) (String.toLower f.name)

friends : List Friend
friends = [
  f "Ayman"
  , f "Jesus"
  , f "Dave"
  , f "DJ"
  , f "Daniel"
  , f "Dean"
  ]

f : String -> Friend
f a = Friend a ""

mkChoices : String -> List Friend
mkChoices q =
  case q of
      "" -> []
      s -> List.filter (matches s) friends

withDebug update action model =
  Debug.watch "State" (update action model)

withLast update action model =
    let m2 = update action model
    in
    {m2 | lastAction <- action}
