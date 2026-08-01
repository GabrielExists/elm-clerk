module HostFunctions exposing (..)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Html
import InterpreterTypes exposing (Value)
import Kernel exposing (InSelector, OutSelector, Selector, To)
import Kernel.Html
import Types exposing (Output, OutputError(..), OutputValue(..))
import Value


constant :
    OutSelector out xo
    -> out
    -> (List Value -> Output)
constant output f =
    \_ ->
        f
            |> output.toValue
            |> OutputValue
            |> Ok


one :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> out)
    -> (List Value -> Output)
one firstSelector _ output f =
    \args ->
        case args of
            [ firstArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        f first
                            |> output.toValue
                            |> OutputValue
                            |> Ok

            _ ->
                Err <| OutputError <| "Expected 1 argument, got " ++ String.fromInt (List.length args)


oneWithViewer :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> out)
    -> (a -> Html.Html msg)
    -> (List Value -> Output)
oneWithViewer firstSelector _ output f viewer =
    \args ->
        case args of
            [ firstArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        Err <| OutputError <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        f first
                            |> output.toValue
                            |> OutputValue
                            |> Ok

            _ ->
                Err <| OutputError <| "Expected 1 argument, got " ++ String.fromInt (List.length args)


two :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> out)
    -> (List Value -> Output)
two firstSelector secondSelector _ output f =
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
                                    |> output.toValue
                                    |> OutputValue
                                    |> Ok

            _ ->
                Err <| OutputError <| "Expected 2 arguments, got " ++ String.fromInt (List.length args)


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> out)
    -> (List Value -> Output)
three firstSelector secondSelector thirdSelector _ output f =
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
                                            |> output.toValue
                                            |> OutputValue
                                            |> Ok

            _ ->
                Err <| OutputError <| "Expected 3 arguments, got " ++ String.fromInt (List.length args)


four :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> InSelector d xd
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> d -> out)
    -> (List Value -> Output)
four firstSelector secondSelector thirdSelector fourthSelector _ output f =
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
                                                    |> output.toValue
                                                    |> OutputValue
                                                    |> Ok

            _ ->
                Err <| OutputError <| "Expected 4 arguments, got " ++ String.fromInt (List.length args)


html : OutSelector (Html.Html msg) {}
html =
    { fromValue =
        \value ->
            Kernel.html.fromValue value |> Maybe.map Kernel.Html.htmlToReal
    , name = "Html.Html"
    }
