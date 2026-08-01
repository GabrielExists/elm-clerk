module Viewers exposing (..)

import Html exposing (Html)
import InterpreterTypes exposing (Value)


viewers : List ( String, List (Value -> Maybe (Html never)) )
viewers =
    []
