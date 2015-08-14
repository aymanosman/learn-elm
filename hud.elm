module Hud where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (..)
import Json.Decode as Json
import Debug exposing (log)

type alias Model =
  { query : String
  , result : String
  }

type alias Input = (String, String)

main : Signal Html
main =
  Signal.map (view queries.address) model

model : Signal Model
model =
  Signal.foldp update init input

input : Signal Input
input =
  Signal.map2 (,) queries.signal results.signal

update : (String, String) -> Model -> Model
update (q, r) m =
  { m |
    query <- q
  , result <- r
  }

init : Model
init =
  { query = ""
  , result = ""
  }

view : Signal.Address String -> Model -> Html
view addr m =
  div []
  [ Html.input
    [value m.query
    , on "input" targetValue (Signal.message addr)
    ] []
  , text m.result
  ]

queries : Signal.Mailbox String
queries = Signal.mailbox "12345"

results : Signal.Mailbox String
results = Signal.mailbox ""

port httpQueries : Signal (Task Http.Error ())
port httpQueries =
  let
    base = "http://api.zippopotam.us/us/"
    f q = Http.getString (base ++ (log "q" q))
    `andThen` report
  in
  Signal.map f queries.signal

report : String -> Task a ()
report = Signal.send results.address
