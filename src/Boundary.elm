module Boundary exposing (..)

import Element exposing (Element)
import Element.Font as Font
import Element.Input
import Elm
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import FastDict as Dict exposing (Dict)
import InterpreterTypes exposing (Value(..))
import Kernel
import List.Extra
import Result.Extra
import ToString exposing (annotationToString, patternToString)
import Types exposing (FrontendMsg(..), FunctionName, OutputError(..), ParameterName(..), RawInteractiveValue(..), TypeName(..))
import Value



-- SELECTORS
-- This is helping generating glue for host code, the boundary between elm-clerk main code and the pages


getSelectorFromTypeName : TypeName -> Elm.Expression
getSelectorFromTypeName (TypeName typeName) =
    case typeName of
        "Bool" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "bool"
                , annotation = Nothing
                }

        "Char" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "char"
                , annotation = Nothing
                }

        "Int" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "int"
                , annotation = Nothing
                }

        "Float" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "float"
                , annotation = Nothing
                }

        "String" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "string"
                , annotation = Nothing
                }

        "Html msg" ->
            Elm.value
                { importFrom = [ "Kernel" ]
                , name = "html"
                , annotation = Nothing
                }

        _ ->
            case removeStartsWith "List " typeName of
                Just inner ->
                    Elm.apply
                        (Elm.value
                            { importFrom = [ "Kernel" ]
                            , name = "list"
                            , annotation = Nothing
                            }
                        )
                        [ getSelectorFromTypeName
                            (TypeName inner)
                        ]

                _ ->
                    Elm.value
                        { importFrom = [ "Kernel" ]
                        , name = "unknown"
                        , annotation = Nothing
                        }


removeStartsWith : String -> String -> Maybe String
removeStartsWith needle haystack =
    if String.startsWith needle haystack then
        Just <| String.dropLeft (String.length needle) haystack

    else
        Nothing



-- INTERACTIVE
-- This is the boundary between text written into an interactive text field and an interpreted value, ready for application


type alias InteractiveElement =
    { key : TypeName
    , conversion : RawInteractiveValue -> Result OutputError Value
    , element : FunctionName -> ParameterName -> Maybe RawInteractiveValue -> Element FrontendMsg
    }


typeNodeMap : Dict String InteractiveElement
typeNodeMap =
    [ interactiveElementInt
    , interactiveElementChar
    , interactiveElementString
    ]
        |> List.map
            (\x ->
                ( let
                    (TypeName key) =
                        x.key
                  in
                  key
                , x
                )
            )
        |> Dict.fromList


interactiveElementInt : InteractiveElement
interactiveElementInt =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            text
                |> String.toInt
                |> Result.fromMaybe "Couldn't parse!"
                |> Result.map Kernel.int.toValue
                |> Result.mapError OutputError
    in
    { key = TypeName "Int"
    , conversion = conversion
    , element = viewTextInput "Int"
    }


interactiveElementChar : InteractiveElement
interactiveElementChar =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            (if String.length text == 1 then
                String.toList text |> List.head

             else
                Nothing
            )
                |> Result.fromMaybe "Not exactly one character"
                |> Result.map Kernel.char.toValue
                |> Result.mapError OutputError
    in
    { key = TypeName "Char"
    , conversion = conversion
    , element = viewTextInput "Char"
    }


interactiveElementString : InteractiveElement
interactiveElementString =
    let
        conversion : RawInteractiveValue -> Result OutputError Value
        conversion (RawInteractiveValue text) =
            text
                |> Kernel.string.toValue
                |> Ok
    in
    { key = TypeName "String"
    , conversion = conversion
    , element = viewTextInput "String"
    }


viewTextInput : String -> FunctionName -> ParameterName -> Maybe RawInteractiveValue -> Element FrontendMsg
viewTextInput typeName functionName (ParameterName parameterName) maybeRawValue =
    let
        maybeValue : Maybe String
        maybeValue =
            maybeRawValue |> Maybe.map (\(RawInteractiveValue x) -> x)
    in
    Element.row [ Element.padding 6 ]
        [ Element.Input.text
            []
            { onChange = \x -> InteractiveUpdated ( functionName, ParameterName parameterName ) (RawInteractiveValue x)
            , text = Maybe.withDefault "" maybeValue
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove
                    [ Font.family
                        [ Font.typeface "Fira Code"
                        , Font.monospace
                        ]
                    , Font.size 16
                    , Element.paddingXY 6 0
                    ]
                    (Element.text (parameterName ++ " : " ++ typeName ++ " = " ++ Maybe.withDefault "" maybeValue))
            }
        ]



-- PARSE TOGETHER
-- The boundary between the plaintext page code and lists of parameters, either as type names, binding names or values


parseReturnType : List TypeAnnotation -> Result OutputError TypeName
parseReturnType ann =
    List.Extra.last ann
        |> Result.fromMaybe (OutputError "No last part of annotation")
        |> Result.map annotationToString
        |> Result.map TypeName


parseTogether : List Pattern -> Declaration -> Int -> Result OutputError ( List ( ParameterName, TypeName ), TypeName )
parseTogether patterns declaration numApplied =
    declarationTypeAnnotation declaration
        |> Result.map bigAnnotationToList
        |> Result.andThen
            (\ann ->
                List.map2 Tuple.pair (List.drop numApplied patterns) ann
                    |> Result.Extra.combineMap parseTogetherSingle
                    |> Result.map List.concat
                    |> Result.andThen
                        (\list ->
                            parseReturnType ann
                                |> Result.map (\returnType -> ( list, returnType ))
                        )
            )


parseTogetherSingle : ( Pattern, TypeAnnotation ) -> Result OutputError (List ( ParameterName, TypeName ))
parseTogetherSingle ( pattern, annotation ) =
    case ( pattern, annotation ) of
        ( TuplePattern patterns, TypeAnnotation.Tupled annotations ) ->
            List.map2 Tuple.pair (patterns |> List.map Node.value) (annotations |> List.map Node.value)
                |> Result.Extra.combineMap parseTogetherSingle
                |> Result.map List.concat

        --( RecordPattern patterns, TypeAnnotation.Record record ) ->
        --    Err "record not implemented"
        --(UnConsPattern head rest, TypeAnnotation. )->
        --ListPattern (List (Node Pattern))
        --VarPattern String
        ( VarPattern binding, TypeAnnotation.Typed moduleNode patterns ) ->
            let
                typeName : String
                typeName =
                    moduleNode
                        |> Node.value
                        |> (\( moduleName, name ) -> moduleName ++ [ name ])
                        |> String.join "."

                fullTypeName : String
                fullTypeName =
                    typeName
                        :: (patterns
                                |> List.map Node.value
                                |> List.map annotationToString
                           )
                        |> String.join " "
            in
            Ok [ ( ParameterName binding, TypeName fullTypeName ) ]

        --NamedPattern QualifiedNameRef (List (Node Pattern))
        --AsPattern (Node Pattern) (Node String)
        --ParenthesizedPattern (Node Pattern)
        ( p, a ) ->
            Err (OutputError ("Can't handle variables of type " ++ (a |> annotationToString) ++ " with binding " ++ (p |> patternToString)))


parseValuesTogether : List Pattern -> List Value -> Result OutputError (List ( ParameterName, Value ))
parseValuesTogether patterns alreadyApplied =
    List.map2 Tuple.pair patterns alreadyApplied
        |> Result.Extra.combineMap parseValuesTogetherSingle
        |> Result.map List.concat


parseValuesTogetherSingle : ( Pattern, Value ) -> Result OutputError (List ( ParameterName, Value ))
parseValuesTogetherSingle ( pattern, rootValue ) =
    case ( pattern, rootValue ) of
        ( TuplePattern [ firstBinding, secondBinding ], Tuple first second ) ->
            [ ( Node.value firstBinding, first ), ( Node.value secondBinding, second ) ]
                |> Result.Extra.combineMap parseValuesTogetherSingle
                |> Result.map List.concat

        ( VarPattern binding, value ) ->
            Ok [ ( ParameterName binding, value ) ]

        ( p, v ) ->
            Err (OutputError ("Can't apply variables with value " ++ (v |> Value.toString) ++ " with binding " ++ (p |> patternToString)))


declarationTypeAnnotation : Declaration -> Result OutputError TypeAnnotation
declarationTypeAnnotation declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .signature
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (OutputError "No type annotations found."))
                |> Result.map Node.value
                |> Result.map .typeAnnotation
                |> Result.map Node.value

        _ ->
            Err (OutputError "Not a function declaration")


bigAnnotationToList : TypeAnnotation -> List TypeAnnotation
bigAnnotationToList annotation =
    case annotation of
        TypeAnnotation.FunctionTypeAnnotation first second ->
            (first
                |> Node.value
            )
                :: (second
                        |> Node.value
                        |> bigAnnotationToList
                   )

        other ->
            [ other ]
