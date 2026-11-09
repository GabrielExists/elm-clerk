module Page4 exposing (..)

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


xValue : Int
xValue =
    3


yValue : Int
yValue =
    7


add : Int -> (Int -> Int)
add a b =
    a + b


added : Int
added =
    add xValue yValue


repeat : Int -> String -> String
repeat num text =
    String.repeat num text


repeat2 : Int -> String -> String
repeat2 =
    String.repeat


list : List Int
list =
    [ 1, 2, 3, 14 ]


stringList : List String
stringList =
    List.map String.fromInt list


increment : Int -> Int
increment value =
    value + 1


incrementTwo : Int -> Int
incrementTwo banana =
    banana + 1


incrementThree : Int -> Int
incrementThree =
    \banana -> banana + 1


incrementedOne : List Int
incrementedOne =
    List.map increment list


incrementedTwo : List Int
incrementedTwo =
    List.map (\x -> x + 1) list


parenthesized : String -> String
parenthesized text =
    "(" ++ text ++ ")"


parenthesizedList : List String
parenthesizedList =
    List.map parenthesized (List.map String.fromInt list)


parenthesizedListTwo : List String
parenthesizedListTwo =
    list
        |> List.map increment
        |> List.map String.fromInt
        |> List.map parenthesized



-- Bavarian


foldl : (a -> b -> b) -> b -> List a -> b
foldl =
    List.foldl


minus a b =
    a - b
