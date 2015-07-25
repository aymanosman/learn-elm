import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Debug exposing (log)
import Json.Decode as Json
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
  | ClickSelect Friend -- was Id and compiler wasn't helping -- change to Int and pick from Dict
  | EnterSelect
  | Next
  | Prev

type alias Id = String

-- Update

update : Action -> Model -> Model
update action model =
  let select f m =
        {m | query <- f.name, selected <- Just f, choices <- []}
  in
  case action of
    NoOp -> model
    Query t ->
      {model | selected <- Nothing, highlighted <- 1, query <- t, choices <- mkChoices model.query}
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

withDebug update action model = (update action model) |> Debug.watch "State"

withLast update action model =
    let m2 = update action model
    in
    {m2 | lastAction <- action}

-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let handleKeyDown f =
        onWithOptions "keydown" {preventDefault = True, stopPropagation = False}
        (Json.customDecoder keyCode (\k ->
            if List.member k [13, 38, 40]
            then Ok k
            else Err "not handling that key"))
        f

      qInput =
        input
          [on "input" targetValue (Signal.message address << Query)
          , handleKeyDown (\k -> Signal.message address <|
                case k of
                    38 -> Prev
                    40 -> Next
                    13 -> EnterSelect
                    _ -> NoOp)
          , value model.query
          , autofocus True
          ] []
      handleSelect f = onClick address (ClickSelect f)
      choiceList =
          let
            tagged = mkTagged model.query
            rendered =
                Dict.values <|
                Dict.map
                (\k v -> viewFriend handleSelect model.highlighted (k,v))
                tagged

        in
        ul [] rendered
  in
  case model.selected of
      Nothing -> div [] [qInput, choiceList]
      Just f -> div [] [qInput, text f.name]

viewFriend : (Friend -> Attribute) -> Int -> (Int, Friend) -> Html
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
mkTagged : String -> Dict Int Friend
mkTagged q =
    let filtered = mkChoices q
    in
    Dict.fromList <| List.map2 (,) [1..List.length filtered] filtered

