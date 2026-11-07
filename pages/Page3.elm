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


convertThing banana multiplier =
    String.length banana * multiplier


viewerInt : Int -> Html.Html never
viewerInt int =
    Html.text (String.fromInt int)
