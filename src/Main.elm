module Main exposing (..)

import Html.Styled exposing (Html, div, styled)
import Css exposing (..)
import Editor
import Window
import Types exposing (Model, Msg(..))


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { viewport = { width = 100, height = 100 }
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( { model | viewport = size }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewport =
    styled div
        [ width (vh 100)
        , height (vh 100)
        ]


view : Model -> Html Msg
view model =
    viewport [] [ Editor.view model.viewport ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.Styled.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
