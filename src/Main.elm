module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Game exposing (EndType(..))
import Game.End as End
import Game.Running as Running
import Game.Setup as Setup
import Random exposing (Seed)


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
    = Loading
    | GameSetup Setup.Model
    | GameRunning Running.Model
    | GameEnd End.Model



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GenerateRandomSeed Random.independentSeed
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameRunning m ->
            Running.subscriptions m |> Sub.map GameRunningMessage

        _ ->
            Sub.none



---- UPDATE ----


type Msg
    = GenerateRandomSeed Seed
    | GameSetupMessage Setup.Msg
    | GameRunningMessage Running.Msg
    | GameEndMessage End.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GenerateRandomSeed seed, Loading ) ->
            ( Setup.init seed |> GameSetup, Cmd.none )

        ( GameSetupMessage m, GameSetup mod ) ->
            ( case Setup.update m mod of
                ( True, newMod ) ->
                    GameRunning (Running.init newMod)

                ( False, newMod ) ->
                    GameSetup newMod
            , Cmd.none
            )

        ( GameRunningMessage m, GameRunning mod ) ->
            ( case Running.update m mod of
                ( Nothing, newMod ) ->
                    GameRunning newMod

                ( Just endType, newMod ) ->
                    End.init newMod endType |> GameEnd
            , Cmd.none
            )

        ( GameEndMessage m, GameEnd mod ) ->
            case End.update m mod of
                Just newMod ->
                    ( GameEnd newMod
                    , Cmd.none
                    )

                Nothing ->
                    ( Loading
                    , Random.generate GenerateRandomSeed Random.independentSeed
                    )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Planet Bob"
    , body =
        [ viewBody model
            |> el
                [ Background.color (rgba 1 1 1 0.75)
                , padding 16
                ]
            |> layout
                [ padding 16
                , Background.image "/static/Cylinder_Interior_Ship.jpg"
                ]
        ]
    }


viewBody : Model -> Element Msg
viewBody model =
    case model of
        Loading ->
            "Loading..."
                |> text
                |> el [ centerX, centerY ]

        GameSetup m ->
            Setup.view m
                |> Element.map GameSetupMessage

        GameRunning m ->
            Running.view m
                |> Element.map GameRunningMessage

        GameEnd m ->
            End.view m
                |> Element.map GameEndMessage
