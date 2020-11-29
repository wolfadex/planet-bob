module Game.Feature exposing (Feature(..), map, withDefault)


type Feature a
    = Uninstalled
    | Installed a


map : (a -> b) -> Feature a -> Feature b
map fn feature =
    case feature of
        Uninstalled ->
            Uninstalled

        Installed a ->
            Installed (fn a)


withDefault : a -> Feature a -> a
withDefault defaultVal feature =
    case feature of
        Uninstalled ->
            defaultVal

        Installed a ->
            a
