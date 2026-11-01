module List.MyExtra exposing (deduplicateByKey, groupBy, iterateN)

import FastSet as Set exposing (Set)
import List.Extra


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy f list =
    list
        |> List.Extra.groupWhile (\l r -> f l == f r)
        |> List.map (\( head, tail ) -> ( f head, head :: tail ))


iterateN : Int -> (a -> a) -> a -> List a
iterateN n f x =
    iterateNHelp n f x [] |> List.reverse


iterateNHelp : Int -> (a -> a) -> a -> List a -> List a
iterateNHelp n f x acc =
    case n of
        0 ->
            acc

        _ ->
            iterateNHelp (n - 1) f (f x) (x :: acc)


deduplicateByKey : (a -> comparable) -> List a -> List a
deduplicateByKey keySelector list =
    List.foldr (deduplicateByKeyAux keySelector) ( Set.empty, [] ) list
        |> Tuple.second


deduplicateByKeyAux : (a -> comparable) -> a -> ( Set comparable, List a ) -> ( Set comparable, List a )
deduplicateByKeyAux keySelector item ( set, output ) =
    let
        key =
            keySelector item
    in
    if Set.member key set then
        ( set, output )

    else
        ( Set.insert key set, item :: output )
