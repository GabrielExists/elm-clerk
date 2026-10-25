module DefaultViewers exposing (..)

import Html exposing (Html)
import InterpreterTypes exposing (Value)


viewerString : String -> Html msg
viewerString string =
    Html.text string
