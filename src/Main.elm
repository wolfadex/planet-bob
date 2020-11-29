module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
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
    | GameOver End.Model



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GenerateRandomSeed Random.independentSeed
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type Msg
    = GenerateRandomSeed Seed
    | SetupMessage Setup.Msg
    | RunningMessage Running.Msg
    | GameOverMessage End.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GenerateRandomSeed seed, Loading ) ->
            ( Setup.init seed |> GameSetup, Cmd.none )

        ( SetupMessage m, GameSetup mod ) ->
            ( case Setup.update m mod of
                ( True, newMod ) ->
                    case Running.init newMod of
                        Just runningMod ->
                            GameRunning runningMod

                        Nothing ->
                            End.init newMod |> GameOver

                ( False, newMod ) ->
                    GameSetup newMod
            , Cmd.none
            )

        ( RunningMessage m, GameRunning mod ) ->
            ( case Running.update m mod of
                ( False, newMod ) ->
                    GameRunning newMod

                ( True, newMod ) ->
                    End.init newMod |> GameOver
            , Cmd.none
            )

        ( GameOverMessage m, GameOver mod ) ->
            ( End.update m mod
                |> GameOver
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Planet Bob"
    , body = [ viewBody model |> layout [ padding 16 ] ]
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
                |> Element.map SetupMessage

        GameRunning m ->
            Running.view m
                |> Element.map RunningMessage

        GameOver m ->
            End.view m
                |> Element.map GameOverMessage
