module Game.Running exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Border as Border
import Game exposing (Ship)
import Game.Feature as Feature exposing (Feature(..))
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
    , actionMessage : String
    }


type alias Card =
    { title : String
    , description : String
    , options : Nonempty Option
    }


type alias Option =
    { label : String
    , action : Ship -> ( Ship, String )
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
            , actionMessage = "Your beautiful new ship launches into the depths of space. Looking for a new world."
            }
        )
        maybeCurrentCard



---- UPDATE ----


type Msg
    = CardOptionSelected (Ship -> ( Ship, String ))


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
                    let
                        ( nextShip, actionMessage ) =
                            fn model.ship
                    in
                    ( False
                    , { ship = nextShip
                      , seed = nextSeed
                      , futureCards = remainingCards
                      , pastCards = model.currentCard :: model.pastCards
                      , currentCard = currentCard
                      , actionMessage = actionMessage
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



---- VIEW ----


view : Model -> Element Msg
view { currentCard, ship, actionMessage } =
    column
        [ spacing 16 ]
        [ actionMessage
            |> text
            |> el
                [ Border.solid
                , Border.widthEach
                    { top = 0
                    , bottom = 2
                    , left = 0
                    , right = 0
                    }
                , padding 8
                ]
        , currentCard
            |> .description
            |> text
            |> List.singleton
            |> paragraph [ padding 8 ]
        , currentCard
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
        , ship
            |> .passengers
            |> String.fromInt
            |> (++) "Passengers: "
            |> text
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
                                ( { ship | passengers = ship.passengers - 5 }, "The asteroids break through the ship, killing 5 passengers." )

                            Installed _ ->
                                ( ship, "Your shields protect the ship." )
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
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Sneak past the ship"
                      , action =
                            \ship ->
                                ( ship
                                , "You pass quietly by, trying not to alert them to your presence"
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
                    )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Leave the moon be"
                      , action = \ship -> ( ship, "You ignore the moon and continue on your way." )
                      }
                    ]
      }
    , { title = "SOS"
      , description = "You hear a SOS come across the radio. A ship not too far from you has run out of food and it's passengers are beginning to starve."
      , options =
            { label = "Send food"
            , action = \ship -> ( ship, "You send them some of your food. They are very grateful." )
            }
                |> List.Nonempty.singleton
                |> List.Nonempty.appendList
                    [ { label = "Take their remaining food"
                      , action = \ship -> ( ship, "You steal the last of their food, leaving them to starve." )
                      }
                    , { label = "You have no food to spare (truth)"
                      , action = \ship -> ( ship, "You greet them and let them know you have no food to spare. Hopefully they find some soon." )
                      }
                    , { label = "You have no food to spare (lie)"
                      , action = \ship -> ( ship, "You greet them and lie about not having any food to spare. Hopefully they find food soon." )
                      }
                    , { label = "Ignore them"
                      , action = \ship -> ( ship, "You ignore the distress signal and continue on your way." )
                      }
                    ]
      }
    ]
