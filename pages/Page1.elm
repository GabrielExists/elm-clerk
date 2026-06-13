module Page1 exposing (..)

import Element
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import InterpreterTypes exposing (Value)
import Kernel
import Rope
import Types exposing (FrontendMsg)


viewers : List ( String, Value -> Maybe (Html never) )
viewers =
    [ ( "Int"
      , \value ->
            Kernel.int.fromValue value
                |> Maybe.map
                    (\number ->
                        String.fromInt number |> Html.text
                    )
      )
    ]


viewerTest : Html never
viewerTest =
    Html.div [ style "display" "inline-block", style "background-color" "black", style "width" "16px", style "height" "16px" ] []


viewerTest2 : String -> Html never
viewerTest2 color =
    Html.div [ style "display" "inline-block", style "background-color" color, style "width" "16px", style "height" "16px" ] []


repeatThrice : String -> String
repeatThrice =
    String.repeat 3


repeat : Int -> String -> String
repeat =
    String.repeat


add : Int -> Int -> Int
add a b =
    a + b


increment : Int -> Int
increment =
    add 1


pow : Int -> Int
pow x =
    x * x


graphFunc : Int -> Int -> (Int -> b) -> List b
graphFunc start end func =
    List.range start end |> List.map func


powGraph : Int -> List Int
powGraph max =
    graphFunc 1 max pow
