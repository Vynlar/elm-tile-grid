module Editor exposing (view)

import Html.Styled exposing (Html, text)
import Css exposing (..)
import Svg.Styled exposing (rect, svg, styled)
import Svg.Styled.Attributes as Attr
import Svg.Styled.Events as Events
import Types exposing (Model, Msg(..), Position(..), Tile(..), TileType(..))
import Array exposing (Array)
import Grid exposing (Grid)


getPosition : Tile -> ( Int, Int )
getPosition t =
    case t of
        Tile _ (Position x y) ->
            ( x, y )


getColor : Tile -> String
getColor t =
    case t of
        Tile Filled _ ->
            "white"

        Tile Empty _ ->
            "rgb(230, 230, 230)"


tileSize : Int
tileSize =
    1


tile =
    styled rect
        [ width (px <| toFloat tileSize)
        , height (px <| toFloat tileSize)
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
        tile
            [ Attr.stroke "rgb(100, 100, 100)"
            , Attr.fill <| getColor t
            , Attr.strokeWidth "0.03"
            , Attr.x <| toString x
            , Attr.y <| toString y
            ]
            []


view : Model -> Html Msg
view model =
    let
        zoom =
            model.zoom

        { width, height } =
            model.viewport

        ( offsetX, offsetY ) =
            model.totalOffset

        tiles =
            model.grid |> Grid.toLists |> List.concat

        vbWidth =
            zoom

        vbHeight =
            (toFloat height / toFloat width) * zoom

        vbOffsetX =
            toFloat offsetX / toFloat width * zoom

        vbOffsetY =
            toFloat offsetY / (toFloat width) * zoom

        viewBoxValue =
            [ vbOffsetX, vbOffsetY, vbWidth, vbHeight ]
                |> List.map toString
                |> String.join " "
    in
        svg
            [ Attr.width <| toString width
            , Attr.height <| toString height
            , Attr.viewBox viewBoxValue
            ]
        <|
            List.map renderTile tiles
