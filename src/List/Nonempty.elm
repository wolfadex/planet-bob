module List.Nonempty exposing
    ( Nonempty
    , append
    , appendList
    , fromList
    , head
    , map
    , singleton
    , tail
    , toList
    )


type Nonempty a
    = Nonempty ( a, List a )


fromList : List a -> Maybe (Nonempty a)
fromList ls =
    case ls of
        [] ->
            Nothing

        first :: rest ->
            Just (Nonempty ( first, rest ))


singleton : a -> Nonempty a
singleton first =
    Nonempty ( first, [] )


appendList : List a -> Nonempty a -> Nonempty a
appendList ls (Nonempty ( a, rest )) =
    Nonempty ( a, rest ++ ls )


toList : Nonempty a -> List a
toList (Nonempty ( first, rest )) =
    first :: rest


map : (a -> b) -> Nonempty a -> Nonempty b
map fn (Nonempty ( first, rest )) =
    Nonempty ( fn first, List.map fn rest )


head : Nonempty a -> a
head (Nonempty ( first, _ )) =
    first


tail : Nonempty a -> List a
tail (Nonempty ( _, rest )) =
    rest


append : Nonempty a -> Nonempty a -> Nonempty a
append (Nonempty ( a, aRest )) (Nonempty ( b, bRest )) =
    Nonempty ( a, aRest ++ b :: bRest )
