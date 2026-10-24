module IdDict exposing (..)

import FastDict as Dict
import Types exposing (IdDict(..))


empty : (kind -> String) -> IdDict kind value
empty transform =
    IdDict Dict.empty transform


fromList : (kind -> String) -> List ( kind, value ) -> IdDict kind value
fromList transform input =
    List.foldl (\( key, value ) dict -> insert key value dict) (empty transform) input


insert : kind -> value -> IdDict kind value -> IdDict kind value
insert key value (IdDict dict transform) =
    IdDict (Dict.insert (transform key) value dict) transform


get : kind -> IdDict kind value -> Maybe value
get key (IdDict dict transform) =
    Dict.get (transform key) dict


isEmpty : IdDict kind value -> Bool
isEmpty (IdDict dict _) =
    Dict.isEmpty dict


map : (String -> a -> b) -> IdDict k a -> IdDict k b
map func (IdDict dict transform) =
    IdDict (Dict.map func dict) transform


filterMap : (String -> a -> Maybe b) -> IdDict k a -> IdDict k b
filterMap func (IdDict dict transform) =
    IdDict
        (Dict.foldl
            (\k v acc ->
                case func k v of
                    Just newValue ->
                        Dict.insert k newValue acc

                    Nothing ->
                        acc
            )
            Dict.empty
            dict
        )
        transform


clear : IdDict kind value -> IdDict kind v
clear (IdDict _ transform) =
    IdDict Dict.empty transform
