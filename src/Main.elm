module Main exposing (..)

import Html.Styled exposing (Html, div, styled, Attribute)
import Html.Styled.Events exposing (on)
import Css exposing (..)
import Editor
import Window
import Types exposing (Model, Msg(..), Mode(..), newTile)
import Json.Decode as Json
import Task
import Keyboard
import Mouse
import Grid


---- MODEL ----


mapSize : Int
mapSize =
    25


init : ( Model, Cmd Msg )
init =
    ( { viewport = { width = 600, height = 600 }
      , zoom = toFloat mapSize
      , mouseStart = Nothing
      , currentOffset = ( 0, 0 )
      , totalOffset = ( 0, 0 )
      , mode = Drawing
      , grid = Grid.new mapSize mapSize newTile
      }
    , Task.perform Resize Window.size
    )



---- UPDATE ----


calcZoom : Float -> Int -> Float
calcZoom zoom delta =
    clamp 5 25 (zoom - (toFloat delta * 0.03))


difference : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
difference ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


add : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( { model | viewport = size }, Cmd.none )

        ScrollZoom delta ->
            ( { model | zoom = calcZoom model.zoom delta }, Cmd.none )

        KeyUp keyCode ->
            let
                zoomDelta =
                    case keyCode of
                        -- Zoom in
                        187 ->
                            40

                        -- Zoom out
                        189 ->
                            -40

                        _ ->
                            0

                newMode =
                    case keyCode of
                        32 ->
                            Drawing

                        _ ->
                            model.mode
            in
                ( { model
                    | zoom = calcZoom model.zoom zoomDelta
                    , mode = newMode
                  }
                , Cmd.none
                )

        KeyDown keyCode ->
            let
                newMode =
                    case keyCode of
                        32 ->
                            Panning

                        _ ->
                            model.mode
            in
                ( { model | mode = newMode }, Cmd.none )

        MouseDown { x, y } ->
            ( { model
                | mouseStart = Just ( x, y )
                , currentOffset = ( 0, 0 )
              }
            , Cmd.none
            )

        MouseUp { x, y } ->
            ( { model
                | mouseStart = Nothing
              }
            , Cmd.none
            )

        MouseMove { x, y } ->
            let
                newCurrentOffset =
                    case model.mouseStart of
                        Just start ->
                            difference start ( x, y )

                        Nothing ->
                            model.currentOffset

                newTotalOffset =
                    if model.mode == Panning then
                        add model.totalOffset (difference newCurrentOffset model.currentOffset)
                    else
                        model.totalOffset
            in
                ( { model
                    | totalOffset = newTotalOffset
                    , currentOffset = newCurrentOffset
                  }
                , Cmd.none
                )

        TileClick ( x, y ) ->
            let
                oldTile =
                    Grid.get x y model.grid

                newTile =
                    case oldTile of
                        Just (Types.Tile oldTileType position) ->
                            Types.setType
                                (Types.toggleType oldTileType)
                                (Types.Tile oldTileType position)

                        Nothing ->
                            Debug.crash "Shouldn't ever access outside of the grid..."

                newGrid =
                    Grid.set x y newTile model.grid
            in
                case model.mode of
                    Panning ->
                        ( model, Cmd.none )

                    Drawing ->
                        ( { model | grid = newGrid }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewport : Model -> List (Attribute msg) -> List (Html msg) -> Html msg
viewport model =
    styled div
        [ width (vw 100)
        , height (vh 100)
        , overflow hidden
        , case model.mode of
            Panning ->
                cursor pointer

            Drawing ->
                cursor crosshair
        ]


onWheel : (Int -> msg) -> Attribute msg
onWheel message =
    on "wheel" (Json.map message (Json.at [ "deltaY" ] Json.int))


view : Model -> Html Msg
view model =
    viewport model
        [ onWheel ScrollZoom ]
        [ Editor.view model
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.Styled.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
