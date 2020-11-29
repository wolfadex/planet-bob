module Game.End exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Game exposing (EndType(..), Ship)
import Gui


type alias Model =
    { ship : Ship
    , endType : EndType
    }



---- INIT ----


init : { a | ship : Ship } -> EndType -> Model
init { ship } endType =
    { ship = ship, endType = endType }



---- UPDATE ----


type Msg
    = NewGame


update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        NewGame ->
            Nothing



---- VIEW ----


view : Model -> Element Msg
view { endType } =
    column
        [ centerX, centerY, spacing 16 ]
        [ text <|
            case endType of
                SettleColony description ->
                    description

                Starve description ->
                    description

                ShipDestroyed description ->
                    description

                EndTimes ->
                    "An elder god appears before you, destroying everything in sight."
        , Gui.button
            { label = text "New Game"
            , onPress = Just NewGame
            }
        ]
