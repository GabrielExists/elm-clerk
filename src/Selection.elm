module Selection exposing (..)

import Boundary exposing (InteractiveElement)
import Dict as SlowDict
import Elm
import Elm.Annotation
import Elm.CodeGen exposing (ModuleName)
import Elm.Package
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference
import Elm.TypeInference.Type
import Elm.Version
import FastDict as Dict exposing (Dict)
import IdDict
import InterpreterTypes exposing (Env, Error(..), Eval, PartiallyAppliedFunction(..), Value(..))
import Parser exposing (DeadEnd)
import Result.Extra
import Result.MyExtra
import ToString exposing (errorToString)
import Types exposing (BackendMsg(..), Cell(..), Code(..), Constant, DependencyName, FileName(..), FrontendModel, FrontendMsg(..), FullCode(..), Function, FunctionName(..), IdDict, Interactives(..), Markdown(..), Output, OutputError(..), OutputValue(..), ParameterName(..), ParameterSelection, ParsedSection, RawInteractiveValue(..), Section(..), Selection, ToBackend(..), ToFrontend(..), TypeName(..), Viewer)
import Utilities exposing (extractNameFromDeclarationNode)


makeDeclarationSelections : FullCode -> FullCode -> Elm.TypeInference.Project -> IdDict FunctionName Selection
makeDeclarationSelections fullSource defaultViewersSource project =
    case ( makeViewers fullSource project, makeViewers defaultViewersSource project ) of
        ( Ok ( userFileName, userDeclarations, userViewers ), Ok ( _, defaultDeclarations, defaultViewers ) ) ->
            userDeclarations
                |> List.filterMap (makeDeclarationSelection project (IdDict.overwrite defaultViewers userViewers) userFileName)
                |> IdDict.fromList (\(FunctionName name) -> name)

        _ ->
            IdDict.empty (\(FunctionName name) -> name)


makeDeclarationSelection : Elm.TypeInference.Project -> IdDict TypeName Elm.Expression -> List String -> Node Declaration -> Maybe ( FunctionName, Selection )
makeDeclarationSelection project viewers userFileName declaration =
    case declaration |> Node.value |> extractNameFromDeclarationNode of
        Nothing ->
            Nothing

        Just (Node range name) ->
            case Elm.TypeInference.getType userFileName range project |> Tuple.first of
                Err _ ->
                    Nothing

                Ok inferredType ->
                    let
                        inferredTypeType : Elm.TypeInference.Type.Type
                        inferredTypeType =
                            inferredType

                        annotations : List TypeAnnotation
                        annotations =
                            Elm.TypeInference.Type.toTypeAnnotation inferredType
                                |> Boundary.bigAnnotationToList

                        patterns : List (Node Pattern)
                        patterns =
                            declaration
                                |> Node.value
                                |> Boundary.declarationPatterns

                        maybePairs : Result OutputError ( List ( ParameterName, TypeName ), TypeName )
                        maybePairs =
                            Boundary.parseTogetherFromAnnotations (patterns |> List.map Node.value) annotations

                        --annotations
                    in
                    case maybePairs of
                        Ok ( pairs, return ) ->
                            let
                                ( parameterSelections, parameterErrors ) =
                                    pairs |> List.map makeParameterSelection |> Result.MyExtra.collectBoth

                                selectorsResult : Result String (List Elm.Expression)
                                selectorsResult =
                                    pairs
                                        |> List.map
                                            (\pair ->
                                                Boundary.getSelectorFromTypeName (Tuple.second pair)
                                            )
                                        |> Result.Extra.combine
                            in
                            case selectorsResult of
                                Ok selectors ->
                                    Elm.tuple (Elm.string name)
                                        (Elm.apply
                                            (getCombinerFromNumArgs
                                                (List.length pairs)
                                            )
                                            (selectors
                                                ++ [ Elm.value
                                                        { importFrom = userFileName
                                                        , name = name
                                                        , annotation = Nothing
                                                        }
                                                   , Boundary.getViewerFromTypeName viewers return
                                                   ]
                                            )
                                        )
                                        |> Just

                                Err missingType ->
                                    Nothing

                        _ ->
                            Nothing


makeParameterSelection : IdDict TypeName Elm.Expression -> ( a, TypeName ) -> Result (List String) ParameterSelection
makeParameterSelection viewers pair =
    let
        typeName : TypeName
        typeName =
            Tuple.second pair

        selectorResult : Result String Elm.Expression
        selectorResult =
            Boundary.getSelectorFromTypeName typeName

        typeNodeResult : Result String InteractiveElement
        typeNodeResult =
            IdDict.get typeName Boundary.typeNodeMap |> Result.fromMaybe "No matching type node"

        prependError : Result error value -> List error -> List error
        prependError result list =
            case result of
                Err error ->
                    error :: list

                _ ->
                    list
    in
    case ( selectorResult, typeNodeResult ) of
        ( Ok selector, Ok typeNode ) ->
            Ok { selector = selector, typeNode = typeNode }

        _ ->
            []
                |> prependError selectorResult
                |> prependError typeNodeResult
                |> List.reverse
                |> Err


makeViewers : FullCode -> Elm.TypeInference.Project -> Result Error ( List String, List (Node Declaration), IdDict TypeName Elm.Expression )
makeViewers source project =
    let
        maybeFile : Result Error File
        maybeFile =
            makeFile source

        maybeFileName : List String
        maybeFileName =
            maybeFile
                |> Result.map .moduleDefinition
                |> Result.map Node.value
                |> Result.map Module.moduleName
                |> Result.withDefault []

        declarations : List (Node Declaration)
        declarations =
            Result.toMaybe maybeFile
                |> Maybe.map
                    (\file ->
                        file
                            |> .declarations
                    )
                |> Maybe.withDefault []

        getFunctionInput : Elm.TypeInference.Type.Type -> Maybe Elm.TypeInference.Type.Type
        getFunctionInput inputType =
            case inputType of
                Elm.TypeInference.Type.Function function ->
                    Just function.from

                _ ->
                    Nothing

        returnTypeIsHtml : Elm.TypeInference.Type.Type -> Bool
        returnTypeIsHtml inputType =
            case inputType of
                Elm.TypeInference.Type.Function function ->
                    case function.to of
                        Elm.TypeInference.Type.Named record ->
                            record.name == "Html"

                        _ ->
                            False

                _ ->
                    False

        foldy : Node String -> Result Error (IdDict TypeName Elm.Expression) -> Result Error (IdDict TypeName Elm.Expression)
        foldy (Node range name) acc =
            case acc of
                Ok dict ->
                    if String.startsWith "viewer" name then
                        case
                            Elm.TypeInference.getType maybeFileName range project |> Tuple.first
                        of
                            Ok inferredType ->
                                case
                                    ( inferredType
                                        |> getFunctionInput
                                        |> Maybe.map Elm.TypeInference.Type.toTypeAnnotation
                                        |> Maybe.map ToString.annotationToString
                                        |> Maybe.map TypeName
                                    , returnTypeIsHtml inferredType
                                    )
                                of
                                    ( Just inputType, True ) ->
                                        IdDict.insert
                                            inputType
                                            (Elm.value
                                                { importFrom = maybeFileName
                                                , name = name
                                                , annotation = Nothing
                                                }
                                            )
                                            dict
                                            |> Ok

                                    ( _, _ ) ->
                                        Ok dict

                            Err error ->
                                Err (InferenceInferError error)

                    else
                        Ok dict

                Err error ->
                    Err error

        result : Result Error (IdDict TypeName Elm.Expression)
        result =
            declarations
                |> List.map Node.value
                |> List.filterMap extractNameFromDeclarationNode
                |> List.foldr
                    foldy
                    (IdDict.empty (\(TypeName typeName) -> typeName) |> Ok)
    in
    case result of
        Ok createdViewers ->
            Ok ( maybeFileName, declarations, createdViewers )

        Err error ->
            Err error


makeFile : FullCode -> Result Error Elm.Syntax.File.File
makeFile (FullCode source) =
    let
        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source
    in
    Result.mapError ParsingError file


getCombinerFromNumArgs : Int -> Elm.Expression
getCombinerFromNumArgs numArgs =
    Elm.value
        { importFrom = [ "HostFunctions" ]
        , name =
            case numArgs of
                0 ->
                    "constant"

                1 ->
                    "one"

                2 ->
                    "two"

                3 ->
                    "three"

                4 ->
                    "four"

                _ ->
                    "tooManyArguments"
        , annotation = Just Elm.Annotation.int -- I need to add this somewhere in order for it to not generate an (incorrect) type signature
        }


dependencyNameToString : ( Elm.Package.Name, Elm.Version.Version ) -> String
dependencyNameToString ( name, version ) =
    Elm.Package.toString name ++ Elm.Version.toString version


maybeProject : List FullCode -> List Elm.TypeInference.Dependency -> Result Error Elm.TypeInference.Project
maybeProject sources dependencies =
    let
        result : Result Error (List File)
        result =
            Result.Extra.combine (List.map makeFile sources)
    in
    result
        |> Result.andThen
            (\files ->
                Elm.TypeInference.init
                    { directDependencies = [ "elm/core", "elm/html" ]
                    , allDependencies = dependencies
                    , sourcesToResolveAmbiguity = SlowDict.empty
                    , projectPackageName = Nothing
                    , projectFiles = files
                    }
                    |> Result.mapError InferenceProjectError
            )
