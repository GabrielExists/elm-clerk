module Page1 exposing (output)

import Element
import Html exposing (Html)
import Html.Attributes exposing (style)
import Rope
import Types exposing (FrontendMsg)


applied : Int -> String
applied repetitions =
    repeat '[' ']' repetitions "Word"


applied2 =
    repeatParen 5 "Bird"


repeatParen : Int -> String -> String
repeatParen =
    repeat '(' ')'


repeat : Char -> Char -> Int -> String -> String
repeat front end repetitions body =
    String.fromChar front ++ String.repeat repetitions body ++ String.fromChar end
