module HostFunctions exposing (..)

import Element
import Elm.Syntax.ModuleName exposing (ModuleName)
import Html
import InterpreterTypes exposing (Value)
import Kernel exposing (InSelector, OutSelector, Selector, To)
import Kernel.Html
import Types exposing (FrontendMsg, Output, OutputError(..), OutputValue(..))
import Value


constant :
    out
    -> (out -> Html.Html FrontendMsg)
    -> (List Value -> Output)
constant f viewer =
    \_ ->
        f
            |> viewer
            |> OutputHtml
            |> Ok


one :
    InSelector a xa
    -> (a -> out)
    -> (out -> Html.Html FrontendMsg)
    -> (List Value -> Output)
one firstSelector f viewer =
    \args ->
        case args of
            [ firstArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        f first
                            |> viewer
                            |> OutputHtml
                            |> Ok

            _ ->
                Err <| OutputError <| "Expected 1 argument, got " ++ String.fromInt (List.length args)


two :
    InSelector a xa
    -> InSelector b xb
    -> (a -> b -> out)
    -> (out -> Html.Html FrontendMsg)
    -> (List Value -> Output)
two firstSelector secondSelector f viewer =
    \args ->
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                Err <| OutputError <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                f first second
                                    |> viewer
                                    |> OutputHtml
                                    |> Ok

            _ ->
                Err <| OutputError <| "Expected 2 arguments, got " ++ String.fromInt (List.length args)


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> (a -> b -> c -> out)
    -> (out -> Html.Html FrontendMsg)
    -> (List Value -> Output)
three firstSelector secondSelector thirdSelector f viewer =
    \args ->
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                Err <| OutputError <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                case thirdSelector.fromValue thirdArg of
                                    Nothing ->
                                        Err <| OutputError <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                                    Just third ->
                                        f first second third
                                            |> viewer
                                            |> OutputHtml
                                            |> Ok

            _ ->
                Err <| OutputError <| "Expected 3 arguments, got " ++ String.fromInt (List.length args)


four :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> InSelector d xd
    -> (a -> b -> c -> d -> out)
    -> (out -> Html.Html FrontendMsg)
    -> (List Value -> Output)
four firstSelector secondSelector thirdSelector fourthSelector f viewer =
    \args ->
        case args of
            [ firstArg, secondArg, thirdArg, fourthArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                Err <| OutputError <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                case thirdSelector.fromValue thirdArg of
                                    Nothing ->
                                        Err <| OutputError <| "Expected the third argument to be " ++ thirdSelector.name ++ ", got " ++ Value.toString thirdArg

                                    Just third ->
                                        case fourthSelector.fromValue fourthArg of
                                            Nothing ->
                                                Err <| OutputError <| "Expected the fourth argument to be " ++ fourthSelector.name ++ ", got " ++ Value.toString fourthArg

                                            Just fourth ->
                                                f first second third fourth
                                                    |> viewer
                                                    |> OutputHtml
                                                    |> Ok

            _ ->
                Err <| OutputError <| "Expected 4 arguments, got " ++ String.fromInt (List.length args)


missingViewer : anything -> Html.Html FrontendMsg
missingViewer _ =
    Html.text ""
