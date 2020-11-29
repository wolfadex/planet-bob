module Game exposing (EndType(..), Ship)

import Game.Feature exposing (Feature)


type alias Ship =
    { passengers : Int
    , cryopods : Feature Int
    , shields : Feature Int
    , biofarms : Feature Int
    , sleepingQuarters : Feature Int
    , fissionReactors : Feature Int
    }


type EndType
    = SettleColony String
    | Starve String
    | ShipDestroyed String
    | EndTimes
