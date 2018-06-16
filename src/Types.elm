module Types exposing (Model, Msg(..))


type alias Model =
    { viewport : { width : Int, height : Int }
    }


type Msg
    = Resize Window.Size
    | Zoom
    | NoOp
