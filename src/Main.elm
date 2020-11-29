module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Font as Font
import Gui
import List.Nonempty exposing (Nonempty(..))
import Random exposing (Seed)
import Random.List


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type Model
    = GameSetup SetupModel
    | GameRunning RunningModel
    | GameOver EndModel


type alias SetupModel =
    { ship : Ship
    , money : Int
    , seed : Seed
    }


type alias RunningModel =
    { ship : Ship
    , seed : Seed
    , futureCards : List Card
    , currentCard : Card
    , pastCards : List Card
    }


type alias EndModel =
    { ship : Ship
    }


type alias Card =
    { title : String
    , description : String
    , options : Nonempty Option
    }


type alias Option =
    { label : String
    , action : RunningMsg
    }


type alias Ship =
    { passengers : Int
    , cryopods : Feature Int
    , shields : Feature Int
    , biofarms : Feature Int
    , sleepingQuarters : Feature Int
    , fissionReactors : Feature Int
    }


type Feature a
    = Uninstalled
    | Installed a



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( GameSetup
        { ship =
            { passengers = 0
            , cryopods = Uninstalled
            , shields = Uninstalled
            , biofarms = Uninstalled
            , sleepingQuarters = Uninstalled
            , fissionReactors = Uninstalled
            }
        , money = 10000
        , seed = Random.initialSeed 0
        }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type SetupMsg
    = ToggleCryopods
    | SetCryopodCount String
    | ToggleShields
    | SetShieldStrength String
    | ToggleBiofarms
    | SetBiofarmCount String
    | ToggleFissionReactors
    | SetReactorCount String
    | ToggleSleepingQuarters
    | SetSleepingQuarterCount String
    | LaunchShip


type RunningMsg
    = CardOptionSelected (Ship -> Ship)


type GameOverMsg
    = NoOp


type Msg
    = SetupMessage SetupMsg
    | RunningMessage RunningMsg
    | GameOverMessage GameOverMsg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SetupMessage m, GameSetup mod ) ->
            ( updateSetup m mod, Cmd.none )

        ( RunningMessage m, GameRunning mod ) ->
            ( updateRunning m mod, Cmd.none )

        ( GameOverMessage m, GameOver mod ) ->
            ( updateGameOver m mod, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGameOver : GameOverMsg -> EndModel -> Model
updateGameOver msg model =
    case msg of
        NoOp ->
            GameOver model


updateRunning : RunningMsg -> RunningModel -> Model
updateRunning msg model =
    case msg of
        CardOptionSelected fn ->
            let
                ( ( maybeCurrentCard, remainingCards ), nextSeed ) =
                    nextRandomCard model.seed model.futureCards
            in
            case maybeCurrentCard of
                Just currentCard ->
                    GameRunning
                        { ship = fn model.ship
                        , seed = nextSeed
                        , futureCards = remainingCards
                        , pastCards = model.currentCard :: model.pastCards
                        , currentCard = currentCard
                        }

                Nothing ->
                    GameOver { ship = model.ship }


updateSetup : SetupMsg -> SetupModel -> Model
updateSetup msg model =
    case msg of
        LaunchShip ->
            let
                ( ( maybeCurrentCard, remainingCards ), nextSeed ) =
                    nextRandomCard model.seed defaultFutureCards
            in
            case maybeCurrentCard of
                Just currentCard ->
                    GameRunning
                        { ship = model.ship
                        , seed = nextSeed
                        , futureCards = remainingCards
                        , pastCards = []
                        , currentCard = currentCard
                        }

                Nothing ->
                    GameOver { ship = model.ship }

        ToggleCryopods ->
            updateShip
                (\ship -> { ship | cryopods = updateFeatureToggle 0 ship.cryopods })
                model
                |> GameSetup

        ToggleShields ->
            updateShip
                (\ship -> { ship | shields = updateFeatureToggle 0 ship.shields })
                model
                |> GameSetup

        ToggleBiofarms ->
            updateShip
                (\ship -> { ship | biofarms = updateFeatureToggle 0 ship.biofarms })
                model
                |> GameSetup

        ToggleFissionReactors ->
            updateShip
                (\ship -> { ship | fissionReactors = updateFeatureToggle 0 ship.fissionReactors })
                model
                |> GameSetup

        ToggleSleepingQuarters ->
            updateShip
                (\ship -> { ship | sleepingQuarters = updateFeatureToggle 0 ship.sleepingQuarters })
                model
                |> GameSetup

        SetCryopodCount newCountStr ->
            case String.toInt newCountStr of
                Nothing ->
                    GameSetup model

                Just newCount ->
                    updateShip
                        (\ship -> { ship | cryopods = updateFeatureValue (max 0 newCount) ship.cryopods })
                        model
                        |> GameSetup

        SetShieldStrength newCountStr ->
            case String.toInt newCountStr of
                Nothing ->
                    GameSetup model

                Just newCount ->
                    updateShip
                        (\ship -> { ship | shields = updateFeatureValue (max 0 newCount) ship.shields })
                        model
                        |> GameSetup

        SetBiofarmCount newCountStr ->
            case String.toInt newCountStr of
                Nothing ->
                    GameSetup model

                Just newCount ->
                    updateShip
                        (\ship -> { ship | biofarms = updateFeatureValue (max 0 newCount) ship.biofarms })
                        model
                        |> GameSetup

        SetReactorCount newCountStr ->
            case String.toInt newCountStr of
                Nothing ->
                    GameSetup model

                Just newCount ->
                    updateShip
                        (\ship -> { ship | fissionReactors = updateFeatureValue (max 0 newCount) ship.fissionReactors })
                        model
                        |> GameSetup

        SetSleepingQuarterCount newCountStr ->
            case String.toInt newCountStr of
                Nothing ->
                    GameSetup model

                Just newCount ->
                    updateShip
                        (\ship -> { ship | sleepingQuarters = updateFeatureValue (max 0 newCount) ship.sleepingQuarters })
                        model
                        |> GameSetup


updateFeatureToggle : a -> Feature a -> Feature a
updateFeatureToggle val feature =
    case feature of
        Uninstalled ->
            Installed val

        Installed _ ->
            Uninstalled


updateFeatureValue : a -> Feature a -> Feature a
updateFeatureValue newVal feature =
    case feature of
        Uninstalled ->
            Uninstalled

        Installed _ ->
            Installed newVal


updateShip : (Ship -> Ship) -> { a | ship : Ship } -> { a | ship : Ship }
updateShip fn ({ ship } as m) =
    { m | ship = fn ship }


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
                , action = CardOptionSelected (\ship -> { ship | passengers = ship.passengers - 5 })
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


view : Model -> Document Msg
view model =
    { title = "Planet Bob"
    , body = [ viewBody model |> layout [ padding 16 ] ]
    }


viewBody : Model -> Element Msg
viewBody model =
    case model of
        GameSetup m ->
            viewSetup m
                |> Element.map SetupMessage

        GameRunning m ->
            viewRunning m
                |> Element.map RunningMessage

        GameOver m ->
            viewGameOver m


viewSetup : SetupModel -> Element SetupMsg
viewSetup { ship, money } =
    let
        energyProduced =
            case ship.fissionReactors of
                Uninstalled ->
                    0

                Installed n ->
                    n * 100

        energyConsumed =
            [ case ship.cryopods of
                Uninstalled ->
                    0

                Installed n ->
                    n * 8
            , case ship.shields of
                Uninstalled ->
                    0

                Installed n ->
                    n * 40
            , case ship.biofarms of
                Uninstalled ->
                    0

                Installed n ->
                    n * 16
            , case ship.sleepingQuarters of
                Uninstalled ->
                    0

                Installed n ->
                    n * 32
            ]
                |> List.sum

        excessEnergy =
            energyProduced - energyConsumed

        moneyRequired =
            [ case ship.cryopods of
                Uninstalled ->
                    0

                Installed n ->
                    n * 80
            , case ship.shields of
                Uninstalled ->
                    0

                Installed n ->
                    n * 400
            , case ship.biofarms of
                Uninstalled ->
                    0

                Installed n ->
                    n * 45
            , case ship.fissionReactors of
                Uninstalled ->
                    0

                Installed n ->
                    n * 5000
            , case ship.sleepingQuarters of
                Uninstalled ->
                    0

                Installed n ->
                    n * 10
            ]
                |> List.sum

        moneyRemaining =
            money - moneyRequired
    in
    column
        [ spacing 16 ]
        [ paragraph
            []
            [ text """The year is 3826. Humanity has spread across the solar system and is looking to reach the whole of the Milky Way.

Build your ship!"""
            ]
        , viewShipSetup ship
        , moneyRemaining
            |> String.fromInt
            |> (++) "Available Money: ¥"
            |> text
            |> el
                [ Font.color <|
                    if moneyRemaining < 0 then
                        rgb 1 0 0

                    else
                        rgb 0 0 0
                ]
        , excessEnergy
            |> String.fromInt
            |> (++) "Excess Energy: "
            |> text
            |> el
                [ Font.color <|
                    if excessEnergy < 0 then
                        rgb 1 0 0

                    else
                        rgb 0 0 0
                ]
        , if excessEnergy < 0 then
            Gui.button
                { label = text "Can't Launch, lower energy usage"
                , onPress = Nothing
                }

          else if moneyRemaining < 0 then
            Gui.button
                { label = text "Can't Launch, not enough money to build ship"
                , onPress = Nothing
                }

          else
            Gui.button
                { label = text "Launch Ship"
                , onPress = Just LaunchShip
                }
        ]


viewShipSetup : Ship -> Element SetupMsg
viewShipSetup ship =
    column
        [ spacing 8 ]
        [ viewFeature "Cryopods"
            ToggleCryopods
            (\l v ->
                Gui.textField
                    { label = l
                    , onChange = SetCryopodCount
                    , value = String.fromInt v
                    }
            )
            ship.cryopods
        , viewFeature "Shields"
            ToggleShields
            (\l v ->
                Gui.textField
                    { label = l
                    , onChange = SetShieldStrength
                    , value = String.fromInt v
                    }
            )
            ship.shields
        , viewFeature "Biofarms"
            ToggleBiofarms
            (\l v ->
                Gui.textField
                    { label = l
                    , onChange = SetBiofarmCount
                    , value = String.fromInt v
                    }
            )
            ship.biofarms
        , viewFeature "Fission Reactors"
            ToggleFissionReactors
            (\l v ->
                Gui.textField
                    { label = l
                    , onChange = SetReactorCount
                    , value = String.fromInt v
                    }
            )
            ship.fissionReactors
        , viewFeature "Sleeping Quarters"
            ToggleSleepingQuarters
            (\l v ->
                Gui.textField
                    { label = l
                    , onChange = SetSleepingQuarterCount
                    , value = String.fromInt v
                    }
            )
            ship.sleepingQuarters
        ]


viewFeature : String -> msg -> (String -> a -> Element msg) -> Feature a -> Element msg
viewFeature label toggleFeature featureInput feature =
    case feature of
        Uninstalled ->
            Gui.button
                { onPress = Just toggleFeature
                , label = text ("Enable " ++ label)
                }

        Installed featureValue ->
            row
                [ spacing 8 ]
                [ Gui.button
                    { onPress = Just toggleFeature
                    , label = text ("Disable " ++ label)
                    }
                , featureInput label featureValue
                ]


viewRunning : RunningModel -> Element RunningMsg
viewRunning { currentCard, ship } =
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


viewGameOver : EndModel -> Element Msg
viewGameOver _ =
    text "Game Over"
