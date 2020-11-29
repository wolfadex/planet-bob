module Game.Running exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Game exposing (Feature(..), Ship)
import Gui
import List.Nonempty exposing (Nonempty)
import Random exposing (Seed)
import Random.List


type alias Model =
    { ship : Ship
    , seed : Seed
    , futureCards : List Card
    , currentCard : Card
    , pastCards : List Card
    }


type alias Card =
    { title : String
    , description : String
    , options : Nonempty Option
    }


type alias Option =
    { label : String
    , action : Msg
    }



---- INIT ----


init : { a | ship : Ship, seed : Seed } -> Maybe Model
init { ship, seed } =
    let
        ( ( maybeCurrentCard, remainingCards ), nextSeed ) =
            nextRandomCard seed defaultFutureCards

        passengerTotal =
            [ case ship.cryopods of
                Uninstalled ->
                    0

                Installed n ->
                    n
            , case ship.sleepingQuarters of
                Uninstalled ->
                    0

                Installed n ->
                    n * 4
            ]
                |> List.sum
    in
    Maybe.map
        (\currentCard ->
            { ship = { ship | passengers = passengerTotal }
            , seed = nextSeed
            , futureCards = remainingCards
            , pastCards = []
            , currentCard = currentCard
            }
        )
        maybeCurrentCard



---- UPDATE ----


type Msg
    = CardOptionSelected (Ship -> Ship)


update : Msg -> Model -> ( Bool, Model )
update msg model =
    case msg of
        CardOptionSelected fn ->
            let
                cards =
                    if List.isEmpty model.futureCards then
                        defaultFutureCards

                    else
                        model.futureCards

                ( ( maybeCurrentCard, remainingCards ), nextSeed ) =
                    nextRandomCard model.seed cards
            in
            case maybeCurrentCard of
                Just currentCard ->
                    ( False
                    , { ship = fn model.ship
                      , seed = nextSeed
                      , futureCards = remainingCards
                      , pastCards = model.currentCard :: model.pastCards
                      , currentCard = currentCard
                      }
                    )

                Nothing ->
                    ( True, model )


nextRandomCard : Seed -> List Card -> ( ( Maybe Card, List Card ), Seed )
nextRandomCard seed cards =
    Random.step
        (Random.List.shuffle cards
            |> Random.andThen Random.List.choose
        )
        seed



---- CARDS ----


defaultFutureCards : List Card
defaultFutureCards =
    [ { title = "Asteroid Belt"
      , description = "The ship passes through an asteroid belt!"
      , options =
            List.Nonempty.singleton
                { label = "Push through the asteroid belt"
                , action =
                    CardOptionSelected
                        (\ship ->
                            case ship.shields of
                                Uninstalled ->
                                    { ship | passengers = ship.passengers - 5 }

                                Installed _ ->
                                    ship
                        )
                }
      }
    , { title = "Red Giant"
      , description = "The ship passes too close too a red giant, overheating the ship. Some of the passengers die."
      , options =
            List.Nonempty.singleton
                { label = "Pass by a red giant"
                , action = CardOptionSelected (\ship -> { ship | passengers = ship.passengers - 2 })
                }
      }
    ]



---- VIEW ----


view : Model -> Element Msg
view { currentCard, ship } =
    column
        [ spacing 16 ]
        [ currentCard
            |> .description
            |> text
            |> List.singleton
            |> paragraph []
        , currentCard
            |> .options
            |> List.Nonempty.toList
            |> List.map
                (\{ label, action } ->
                    Gui.button
                        { label = text label
                        , onPress = Just action
                        }
                )
            |> row []
        , ship
            |> .passengers
            |> String.fromInt
            |> (++) "Passengers: "
            |> text
        ]
