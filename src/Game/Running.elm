module Game.Running exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Game exposing (EndType(..), Ship)
import Game.Feature as Feature exposing (Feature(..))
import Gui
import List.Nonempty exposing (Nonempty)
import Random exposing (Seed)
import Random.Extra exposing (result)
import Random.List


type alias Model =
    { ship : Ship
    , seed : Seed
    , futureCards : List Card
    , playSpeed : PlaySpeed
    , elapsedTime : Float
    , yearsElapsed : Int
    , food : Int
    , yearsSinceEvent : Int
    }


type PlaySpeed
    = Paused
    | Normal
    | Double
    | Triple
    | PausedForEvent Card
    | PausedForResult String


type alias Card =
    { title : String
    , description : String
    , options : Nonempty Option
    }


type alias Option =
    { label : String
    , action : Ship -> ( Ship, String, Bool )
    }



---- INIT ----


init : { a | ship : Ship, seed : Seed } -> Model
init { ship, seed } =
    let
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
    { ship = { ship | passengers = passengerTotal }
    , seed = seed
    , futureCards = defaultFutureCards
    , playSpeed =
        PausedForEvent
            { title = "The Journey Begins"
            , description = "The ship sits, ready to launch."
            , options =
                List.Nonempty.singleton
                    { label = "Launch the Ship"
                    , action =
                        \s ->
                            ( s
                            , "Your beautiful new ship launches into the depths of space. Looking for a new world."
                            , False
                            )
                    }
            }
    , elapsedTime = 0
    , yearsElapsed = 0
    , food = 100
    , yearsSinceEvent = 0
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playSpeed of
        Paused ->
            Sub.none

        PausedForEvent _ ->
            Sub.none

        PausedForResult _ ->
            Sub.none

        _ ->
            Browser.Events.onAnimationFrameDelta Tick



---- UPDATE ----


type Msg
    = CardOptionSelected (Ship -> ( Ship, String, Bool ))
    | Tick Float
    | Continue
    | SetPlaySpeed PlaySpeed


update : Msg -> Model -> ( Maybe EndType, Model )
update msg model =
    case msg of
        SetPlaySpeed newSpeed ->
            ( Nothing, { model | playSpeed = newSpeed } )

        Continue ->
            ( Nothing, { model | playSpeed = Normal } )

        Tick deltaTime ->
            ( Nothing
            , case model.playSpeed of
                Normal ->
                    updateTick 1 deltaTime model

                Double ->
                    updateTick 2 deltaTime model

                Triple ->
                    updateTick 3 deltaTime model

                _ ->
                    model
            )

        CardOptionSelected fn ->
            let
                ( nextShip, resultMessage, settleHere ) =
                    fn model.ship
            in
            ( if settleHere then
                Just (SettleColony "You settle a colony on a beautiful Earth like planet. The citizens name it Bob.")

              else
                Nothing
            , { model
                | ship = nextShip
                , playSpeed = PausedForResult resultMessage
              }
            )


updateTick : Int -> Float -> Model -> Model
updateTick playSpeed deltaTime model =
    let
        newElapsedTime =
            model.elapsedTime + deltaTime

        diffedElapsedTime =
            if newElapsedTime - 1000 < 0 then
                newElapsedTime

            else
                newElapsedTime - 1000

        modelWithElapsedTime =
            { model | elapsedTime = diffedElapsedTime }
    in
    if newElapsedTime > diffedElapsedTime then
        updateTickHelper playSpeed modelWithElapsedTime

    else
        modelWithElapsedTime


updateTickHelper : Int -> Model -> Model
updateTickHelper timesToUpdate model =
    if timesToUpdate == 0 then
        model

    else
        let
            foodCreated =
                case model.ship.biofarms of
                    Installed n ->
                        n * 40

                    Uninstalled ->
                        0

            foodConsumed =
                model.ship.passengers * 10

            foodRemaining =
                model.food + foodCreated - foodConsumed

            passengersRemaining =
                if foodRemaining < -20 then
                    model.ship.passengers - 1

                else
                    model.ship.passengers

            ship =
                model.ship

            nextModel =
                { model
                    | yearsElapsed = model.yearsElapsed + 1
                    , yearsSinceEvent = model.yearsSinceEvent + 1
                    , food = foodRemaining
                    , ship = { ship | passengers = passengersRemaining }
                }
        in
        case maybeEvent nextModel of
            ( Just card, finalModel ) ->
                { finalModel | playSpeed = PausedForEvent card }

            ( Nothing, finalModel ) ->
                updateTickHelper (timesToUpdate - 1) finalModel


maybeEvent : Model -> ( Maybe Card, Model )
maybeEvent model =
    let
        ( shouldPullEventCard, nextSeed ) =
            Random.step
                (Random.Extra.oneIn (100 - model.yearsSinceEvent))
                model.seed
    in
    if shouldPullEventCard then
        let
            cards =
                if List.isEmpty model.futureCards then
                    defaultFutureCards

                else
                    model.futureCards

            ( ( maybeCard, remainingCards ), finalSeed ) =
                nextRandomCard nextSeed cards
        in
        case maybeCard of
            Nothing ->
                ( Nothing, { model | seed = finalSeed } )

            Just card ->
                ( Just card
                , { model
                    | seed = finalSeed
                    , futureCards = remainingCards
                    , yearsSinceEvent = 0
                  }
                )

    else
        ( Nothing, { model | seed = nextSeed } )


nextRandomCard : Seed -> List Card -> ( ( Maybe Card, List Card ), Seed )
nextRandomCard seed cards =
    Random.step
        (Random.List.shuffle cards
            |> Random.andThen Random.List.choose
        )
        seed



---- VIEW ----


view : Model -> Element Msg
view { ship, yearsElapsed, food, playSpeed } =
    column
        [ spacing 16 ]
        [ case playSpeed of
            PausedForEvent card ->
                [ card
                    |> .description
                    |> text
                    |> List.singleton
                    |> paragraph [ padding 8 ]
                , card
                    |> .options
                    |> List.Nonempty.toList
                    |> List.map
                        (\{ label, action } ->
                            Gui.button
                                { label = text label
                                , onPress = Just (CardOptionSelected action)
                                }
                        )
                    |> row [ spacing 8 ]
                ]
                    |> column []
                    |> Gui.card []

            PausedForResult resultStr ->
                [ text resultStr
                , Gui.button
                    { label = text "Continue"
                    , onPress = Just Continue
                    }
                ]
                    |> column []
                    |> Gui.card []

            _ ->
                none
        , ship
            |> .passengers
            |> String.fromInt
            |> (++) "Passengers: "
            |> text
        , food
            |> String.fromInt
            |> (++) "Food: "
            |> text
        , yearsElapsed
            |> String.fromInt
            |> (++) "Years: "
            |> text
        , column
            [ spacing 8 ]
            [ text "Play Speed"
            , row
                [ spacing 16 ]
                [ Gui.button
                    { label =
                        el
                            (case playSpeed of
                                Paused ->
                                    [ Font.underline ]

                                _ ->
                                    []
                            )
                            (text "Pause")
                    , onPress = Just (SetPlaySpeed Paused)
                    }
                , Gui.button
                    { label =
                        el
                            (case playSpeed of
                                Normal ->
                                    [ Font.underline ]

                                _ ->
                                    []
                            )
                            (text "Normal")
                    , onPress = Just (SetPlaySpeed Normal)
                    }
                , Gui.button
                    { label =
                        el
                            (case playSpeed of
                                Double ->
                                    [ Font.underline ]

                                _ ->
                                    []
                            )
                            (text "Double")
                    , onPress = Just (SetPlaySpeed Double)
                    }
                , Gui.button
                    { label =
                        el
                            (case playSpeed of
                                Triple ->
                                    [ Font.underline ]

                                _ ->
                                    []
                            )
                            (text "Triple")
                    , onPress = Just (SetPlaySpeed Triple)
                    }
                ]
            ]
        ]



---- CARDS ----


defaultFutureCards : List Card
defaultFutureCards =
    [ { title = "Asteroid Belt"
      , description = "The ship is headed towards an asteroid belt!"
      , options =
            List.Nonempty.singleton
                { label = "Push through the asteroid belt"
                , action =
                    \ship ->
                        case ship.shields of
                            Uninstalled ->
                                ( { ship | passengers = ship.passengers - 5 }
                                , "The asteroids break through the ship, killing 5 passengers."
                                , False
                                )

                            Installed _ ->
                                ( ship
                                , "Your shields protect the ship."
                                , False
                                )
                }
      }
    , { title = "Red Giant"
      , description = "The ship passes too close too a red giant."
      , options =
            List.Nonempty.singleton
                { label = "Pass by a red giant"
                , action =
                    \ship ->
                        ( { ship | passengers = ship.passengers - 2 }
                        , "The heat of the red giant scorches the ship, killing 2 passengers."
                        , False
                        )
                }
      }
    , { title = "Alien Encounter"
      , description = "You pick up an foreign ship on your radar."
      , options =
            { label = "Hail the ship"
            , action =
                \ship ->
                    ( { ship | fissionReactors = Feature.map ((*) 2) ship.fissionReactors }
                    , "The aliens are friendly and show you how to modify your reactor to be twice as powerful. Your energy output doubles!"
                    , False
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Sneak past the ship"
                      , action =
                            \ship ->
                                ( ship
                                , "You pass quietly by, trying not to alert them to your presence"
                                , False
                                )
                      }
                    ]
      }
    , { title = "Rogue Moon"
      , description = "You detect a rogue moon near by."
      , options =
            { label = "Explore the moon"
            , action =
                \ship ->
                    ( { ship | passengers = ship.passengers - 3 }
                    , "You send down 3 passengers to explore the planet. The moon is actually alive! It swallows the small exploration pod, killing all 3 crew."
                    , False
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Leave the moon be"
                      , action =
                            \ship ->
                                ( ship
                                , "You ignore the moon and continue on your way."
                                , False
                                )
                      }
                    ]
      }
    , { title = "SOS"
      , description = "You hear a SOS come across the radio. A ship not too far from you has run out of food and it's passengers are beginning to starve."
      , options =
            { label = "Send food"
            , action =
                \ship ->
                    ( ship
                    , "You send them some of your food. They are very grateful."
                    , False
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Take their remaining food"
                      , action =
                            \ship ->
                                ( ship
                                , "You steal the last of their food, leaving them to starve."
                                , False
                                )
                      }
                    , { label = "You have no food to spare (truth)"
                      , action =
                            \ship ->
                                ( ship
                                , "You greet them and let them know you have no food to spare. Hopefully they find some soon."
                                , False
                                )
                      }
                    , { label = "You have no food to spare (lie)"
                      , action =
                            \ship ->
                                ( ship
                                , "You greet them and lie about not having any food to spare. Hopefully they find food soon."
                                , False
                                )
                      }
                    , { label = "Ignore them"
                      , action =
                            \ship ->
                                ( ship
                                , "You ignore the distress signal and continue on your way."
                                , False
                                )
                      }
                    ]
      }
    , { title = "Planet Bob"
      , description = "A beautiful look planet, almost looks like Earth's twin sister. A few large oceans and some large continents full of lush forests."
      , options =
            { label = "Settle here"
            , action =
                \ship ->
                    ( ship
                    , "You land on the planet, starting a new colony."
                    , True
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Continue on in search of another home"
                      , action =
                            \ship ->
                                ( ship
                                , "This planet just isn't good enough, so you continue your journey."
                                , False
                                )
                      }
                    ]
      }
    ]
