module Game exposing (Ship)

import Game.Feature exposing (Feature)


type alias Ship =
    { passengers : Int
    , cryopods : Feature Int
    , shields : Feature Int
    , biofarms : Feature Int
    , sleepingQuarters : Feature Int
    , fissionReactors : Feature Int
    }
