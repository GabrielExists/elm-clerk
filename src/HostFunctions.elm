module HostFunctions exposing (..)

import Elm.Syntax.ModuleName exposing (ModuleName)
import InterpreterTypes exposing (Value)
import Kernel exposing (InSelector, OutSelector, To)
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
                Err <| OutputError "Wrong number of arguments"


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
                Err <| OutputError "Wrong number of arguments"


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
                Err <| OutputError "Wrong number of arguments"
