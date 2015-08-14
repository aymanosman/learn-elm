import History
import Graphics.Element exposing (Element, show)
import Mouse
import Signal exposing ((<~), (~))
import Task exposing (Task)

type alias Model =
  { counts : Int
  , path : String
  }

type alias Input =
  { path : String
  }

main : Signal Element
main = view <~ model

model : Signal Model
model =
    Model <~ counts ~ History.path

counts : Signal Int
counts = Signal.foldp (+) 0 <| (always 1) <~ Mouse.clicks

paths : Signal.Mailbox String
paths = Signal.mailbox ""

view : Model -> Element
view m = show m

update : Input -> Model -> Model
update i m =
  { m |
    counts <- m.counts + 1
  }

port title : Signal String
port title = makeTitle <~ counts

port runTask : Signal (Task error ())
port runTask = (History.setPath << makeHash) <~ counts

makeTitle n = "n = " ++ toString n

makeHash n = "#/" ++ toString n
