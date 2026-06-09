module Interactives exposing (..)

import FastDict as Dict
import Types exposing (FunctionName(..), Interactives(..), ParameterName(..), RawInteractiveValue(..))


interactivesEmpty : Interactives
interactivesEmpty =
    Interactives Dict.empty


interactivesInsert : ( FunctionName, ParameterName ) -> RawInteractiveValue -> Interactives -> Interactives
interactivesInsert ( FunctionName functionName, ParameterName parameterName ) value (Interactives interactives) =
    Interactives (Dict.insert ( functionName, parameterName ) value interactives)


interactivesGet : ( FunctionName, ParameterName ) -> Interactives -> Maybe RawInteractiveValue
interactivesGet ( FunctionName functionName, ParameterName parameterName ) (Interactives interactives) =
    Dict.get ( functionName, parameterName ) interactives



--interactivesFromList : List ( FunctionName, ParameterName ) -> Interactives
--interactivesFromList list =
--    let
--        unpack : ( FunctionName, ParameterName ) -> RawInteractiveValue -> ( ( String, String ), RawInteractiveValue )
--        unpack ( FunctionName functionName, ParameterName parameterName ) value =
--            ( ( functionName, parameterName ), value )
--    in
--    List.map unpack list
--        |> Dict.fromList
--        |> Interactives
