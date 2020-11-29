module Game.Setup exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Game exposing (Feature(..), Ship)
import Gui
import Random exposing (Seed)


init : Seed -> Model
init seed =
    { ship =
        { passengers = 0
        , cryopods = Uninstalled
        , shields = Uninstalled
        , biofarms = Uninstalled
        , sleepingQuarters = Uninstalled
        , fissionReactors = Uninstalled
        }
    , money = 10000
    , seed = seed
    }


type alias Model =
    { ship : Ship
    , money : Int
    , seed : Seed
    }



---- UPDATE ----


type Msg
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


update : Msg -> Model -> ( Bool, Model )
update msg model =
    case msg of
        ToggleCryopods ->
            updateShip
                (\ship -> { ship | cryopods = updateFeatureToggle 1 ship.cryopods })
                model
                |> Tuple.pair False

        LaunchShip ->
            ( True, model )

        ToggleShields ->
            updateShip
                (\ship -> { ship | shields = updateFeatureToggle 1 ship.shields })
                model
                |> Tuple.pair False

        ToggleBiofarms ->
            updateShip
                (\ship -> { ship | biofarms = updateFeatureToggle 1 ship.biofarms })
                model
                |> Tuple.pair False

        ToggleFissionReactors ->
            updateShip
                (\ship -> { ship | fissionReactors = updateFeatureToggle 1 ship.fissionReactors })
                model
                |> Tuple.pair False

        ToggleSleepingQuarters ->
            updateShip
                (\ship -> { ship | sleepingQuarters = updateFeatureToggle 1 ship.sleepingQuarters })
                model
                |> Tuple.pair False

        SetCryopodCount newCountStr ->
            Tuple.pair False <|
                case String.toInt newCountStr of
                    Nothing ->
                        model

                    Just newCount ->
                        updateShip
                            (\ship -> { ship | cryopods = updateFeatureValue (max 0 newCount) ship.cryopods })
                            model

        SetShieldStrength newCountStr ->
            Tuple.pair False <|
                case String.toInt newCountStr of
                    Nothing ->
                        model

                    Just newCount ->
                        updateShip
                            (\ship -> { ship | shields = updateFeatureValue (max 0 newCount) ship.shields })
                            model

        SetBiofarmCount newCountStr ->
            Tuple.pair False <|
                case String.toInt newCountStr of
                    Nothing ->
                        model

                    Just newCount ->
                        updateShip
                            (\ship -> { ship | biofarms = updateFeatureValue (max 0 newCount) ship.biofarms })
                            model

        SetReactorCount newCountStr ->
            Tuple.pair False <|
                case String.toInt newCountStr of
                    Nothing ->
                        model

                    Just newCount ->
                        updateShip
                            (\ship -> { ship | fissionReactors = updateFeatureValue (max 0 newCount) ship.fissionReactors })
                            model

        SetSleepingQuarterCount newCountStr ->
            Tuple.pair False <|
                case String.toInt newCountStr of
                    Nothing ->
                        model

                    Just newCount ->
                        updateShip
                            (\ship -> { ship | sleepingQuarters = updateFeatureValue (max 0 newCount) ship.sleepingQuarters })
                            model


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



---- VIEW ----


view : Model -> Element Msg
view { ship, money } =
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


viewShipSetup : Ship -> Element Msg
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
