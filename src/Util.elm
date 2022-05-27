module Util exposing (minOfList, some)


some : (a -> Maybe b) -> List a -> Maybe b
some predicate xs =
    case xs of
        [] ->
            Nothing

        a :: rest ->
            case predicate a of
                Just b ->
                    Just b

                Nothing ->
                    some predicate rest


minOfList : List comparable -> Maybe comparable
minOfList xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            let
                min =
                    List.foldl
                        (\n acc ->
                            if n < acc then
                                n

                            else
                                acc
                        )
                        x
                        rest
            in
            Just min
