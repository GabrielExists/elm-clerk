module GetHostText exposing (..)

import Boundary exposing (InteractiveElement)
import Dict as SlowDict
import Elm
import Elm.Annotation
import Elm.Docs exposing (Module)
import Elm.Package
import Elm.Parser
import Elm.Project exposing (Project(..))
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference
import Elm.TypeInference.Type
import Elm.Version
import Http
import IdDict
import InterpreterTypes exposing (Env, Error(..), Eval, PartiallyAppliedFunction(..), Value(..))
import Parser exposing (DeadEnd)
import Result.Extra
import ToString exposing (errorToString)
import Types exposing (BackendMsg(..), Cell(..), Code(..), Constant, DependencyName, DependencyRequest, FileName(..), FrontendModel, FrontendMsg(..), FullCode(..), Function, FunctionName(..), IdDict, Interactives(..), Markdown(..), Output, OutputError(..), OutputValue(..), ParameterName(..), ParsedSection, RawInteractiveValue(..), Section(..), ToBackend(..), ToFrontend(..), TypeName(..), Viewer)
import Utilities exposing (extractNameFromDeclarationNode)


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


getHostText : FullCode -> FullCode -> Elm.TypeInference.Project -> String
getHostText fullSource defaultViewersSource project =
    let
        fileList =
            case ( makeViewers fullSource project, makeViewers defaultViewersSource project ) of
                ( Ok ( userFileName, userDeclarations, userViewers ), Ok ( _, defaultDeclarations, defaultViewers ) ) ->
                    let
                        viewers : IdDict TypeName Elm.Expression
                        viewers =
                            IdDict.overwrite defaultViewers userViewers

                        fileListDeclaration : Elm.Declaration
                        fileListDeclaration =
                            (userDeclarations
                                |> List.filterMap
                                    (\declaration ->
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
                                                                Elm.TypeInference.Type.toTypeAnnotation (Debug.log "Start annotation" inferredType)
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
                                                        selectorsAndMissing maybePairs name userFileName viewers
                                    )
                            )
                                |> Elm.list
                                |> Elm.declaration "functionList"
                    in
                    fileListDeclaration

                ( Err error, _ ) ->
                    Elm.list []
                        |> Elm.declaration "functionList"
                        |> List.singleton
                        |> List.append [ Elm.comment (errorToString error) ]
                        |> Elm.group

                ( _, Err error ) ->
                    Elm.list []
                        |> Elm.declaration "functionList"
                        |> List.singleton
                        |> List.append [ Elm.comment (errorToString error) ]
                        |> Elm.group
    in
    Elm.file [ "Host" ] [ fileList ]
        |> .contents


selectorsAndMissing : Result error ( List ( a, TypeName ), TypeName ) -> String -> List String -> IdDict TypeName Elm.Expression -> Maybe Elm.Expression
selectorsAndMissing maybePairs name userFileName viewers =
    case maybePairs of
        Ok ( pairs, return ) ->
            let
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


checkDependencies : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
checkDependencies model =
    if IdDict.size model.depProjects == List.length model.depNames && IdDict.size model.depDocs == List.length model.depNames then
        ( model
        , Http.get
            { url = "/_x/read/src/DefaultViewers.elm"
            , expect = Http.expectString Open9FetchedDefaults
            }
        )

    else
        ( model, Cmd.none )


combineDependencies : List DependencyName -> IdDict DependencyName Project -> IdDict DependencyName (List Module) -> Result Error (List Elm.TypeInference.Dependency)
combineDependencies depNames depProjects depDocs =
    depNames
        |> List.foldr
            (\name acc ->
                case acc of
                    Err error ->
                        Err error

                    Ok dependencies ->
                        case ( IdDict.get name depProjects, IdDict.get name depDocs ) of
                            ( Just project, Just modules ) ->
                                let
                                    directDeps : List String
                                    directDeps =
                                        case project of
                                            Application applicationInfo ->
                                                applicationInfo.depsDirect
                                                    |> List.map Tuple.first
                                                    |> List.map Elm.Package.toString

                                            Package packageInfo ->
                                                packageInfo.deps
                                                    |> List.map Tuple.first
                                                    |> List.map Elm.Package.toString
                                in
                                { name = Tuple.first name |> Elm.Package.toString
                                , dependencies = directDeps
                                , modules = modules
                                }
                                    :: dependencies
                                    |> Ok

                            ( _, _ ) ->
                                Err (StringError ("Dependency " ++ dependencyNameToString name ++ " was missing either elm.json or docs.json."))
            )
            (Ok [])
