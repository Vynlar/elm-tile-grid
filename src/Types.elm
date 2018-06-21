module Types
    exposing
        ( Model
        , Msg(..)
        , Mode(..)
        , Position(..)
        , Tile(..)
        , TileType(..)
        , newTile
        , setType
        , toggleType
        )

import Window
import Mouse
import Grid exposing (Grid)


type Position
    = Position Int Int


type Tile
    = Tile TileType Position


newTile : ( Int, Int ) -> Tile
newTile ( x, y ) =
    Tile Empty (Position x y)


setType : TileType -> Tile -> Tile
setType newType (Tile _ position) =
    Tile newType position


toggleType : TileType -> TileType
toggleType t =
    case t of
        Filled ->
            Empty

        Empty ->
            Filled


type TileType
    = Filled
    | Empty


type Mode
    = Panning
    | Drawing


type alias Model =
    { viewport :
        { width : Int
        , height : Int
        }
    , zoom : Float
    , mouseStart : Maybe ( Int, Int )
    , currentOffset : ( Int, Int )
    , totalOffset : ( Int, Int )
    , mode : Mode
    , grid : Grid Tile
    }


type Msg
    = Resize Window.Size
    | ScrollZoom Int
    | KeyUp Int
    | KeyDown Int
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | TileClick ( Int, Int )
    | NoOp
