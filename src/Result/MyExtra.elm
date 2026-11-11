module Result.MyExtra exposing (..)

{-| Combine a list of results into two lists, first containing all ok values, second containing all err values
-}


collectBoth : List (Result x a) -> ( List x, List a )
collectBoth list =
    collectBothHelp list [] []


collectBothHelp : List (Result x a) -> List x -> List a -> ( List x, List a )
collectBothHelp list errs oks =
    case list of
        head :: tail ->
            case head of
                Ok a ->
                    collectBothHelp tail errs (a :: oks)

                Err x ->
                    collectBothHelp tail (x :: errs) oks

        [] ->
            ( List.reverse errs, List.reverse oks )
