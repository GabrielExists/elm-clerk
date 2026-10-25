module Page3 exposing (..)

import Element
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import InterpreterTypes exposing (Value)
import Kernel
import List.Extra
import List.MyExtra
import Rope
import Types exposing (FrontendMsg)
import Value


double : Int -> Int
double x =
    2 * x


square : Int -> Int
square x =
    x * x


text : String
text =
    "Hello"


doubleTwice : Int -> Int
doubleTwice x =
    double (double x)


viewerBlock : Int -> Html msg
viewerBlock number =
    let
        color =
            if number > 0 then
                "black"

            else
                "white"
    in
    Html.div
        [ style "display" "inline-block"
        , style "width" "16px"
        , style "height" "16px"
        , style "border-style" "solid"
        , style "border-color" "black"
        , style "border-width" "1.5px"
        , style "background-color" color
        ]
        []
