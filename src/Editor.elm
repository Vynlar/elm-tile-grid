module Editor exposing (view)

import Html.Styled exposing (Html, text)
import Css exposing (..)
import Svg.Styled exposing (rect, svg, styled)
import Svg.Styled.Attributes as SA
import Types exposing (Msg(..))


type Position
    = Position Int Int


type Tile
    = Tile TileType Position


getPosition : Tile -> ( Int, Int )
getPosition t =
    case t of
        Tile _ (Position x y) ->
            ( x, y )


newTile x y =
    Tile Empty (Position x y)


type TileType
    = Filled
    | Empty


type alias Grid a =
    List (List a)


tileSize : Int
tileSize =
    1


mapElements : (a -> b) -> Grid a -> Grid b
mapElements =
    List.map << List.map


mapRows : (List a -> List b) -> Grid a -> Grid b
mapRows f g =
    List.map f g



--emptyGrid : Int -> Int -> Grid Tile


emptyGrid width height =
    let
        row =
            scale <| List.range 0 (width - 1)

        cols =
            scale <| List.range 0 (height - 1)

        scale =
            List.map (\x -> x * tileSize)

        coords : Grid ( Int, Int )
        coords =
            List.map2
                (\colNum row ->
                    List.map (\e -> ( e, colNum )) row
                )
                cols
                (List.repeat height row)

        tiles : Grid Tile
        tiles =
            (List.map << List.map) (\( x, y ) -> newTile x y) coords
    in
        tiles


flatten : Grid a -> List a
flatten =
    List.concat


exampleGrid =
    emptyGrid 25 25


tile =
    styled rect
        [ width (px <| toFloat tileSize)
        , height (px <| toFloat tileSize)
        , fill transparent
        , hover
            [ fill (rgb 200 200 200)
            ]
        ]


renderTile : Tile -> Html Msg
renderTile t =
    let
        ( x, y ) =
            getPosition t
    in
        tile [ SA.stroke "black", SA.strokeWidth "0.05", SA.x <| toString x, SA.y <| toString y ] []


view : { width : Int, height : Int } -> Html Msg
view { width, height } =
    let
        tiles =
            flatten exampleGrid
    in
        svg
            [ SA.width <| toString width
            , SA.height <| toString height
            , SA.viewBox "0 0 25 25"
            , SA.onScroll Zoom
            ]
        <|
            List.map renderTile tiles
