module Game exposing (Feature(..), Ship)


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
