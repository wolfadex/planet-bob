module Game.End exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Game exposing (Ship)


type alias Model =
    { ship : Ship
    }



---- INIT ----


init : { a | ship : Ship } -> Model
init { ship } =
    { ship = ship }



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



---- VIEW ----


view : Model -> Element Msg
view _ =
    text "Game Over"
