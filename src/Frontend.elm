module Frontend exposing (Model, app)

import Boundary exposing (InteractiveElement, declarationTypeAnnotation)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Common exposing (notifyIn)
import DefaultViewers
import Element exposing (Attribute, Element, clipX, el, fill, height, maximum, paddingEach, paragraph, scrollbarX, scrollbarY, shrink, text, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Lazy
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Op
import Elm.Parser
import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.ToString
import Environment
import Eval.Expression
import Eval.Module
import EvalResult
import FastDict as Dict exposing (Dict)
import Hash
import Host
import Html
import Html.Attributes
import Http
import IdDict
import Interactives
import InterpreterTypes exposing (Env, Error(..), Eval, PartiallyAppliedFunction(..), Value(..))
import Json.Decode
import Kernel
import Kernel.Html
import Lamdera exposing (sendToBackend)
import List.Extra exposing (Step(..))
import Markdown.Parser
import Markdown.Renderer
import Markdown.Renderer.ElmUi
import Maybe.Extra
import Parser exposing (DeadEnd)
import ParserFast
import Regex
import Result.Extra
import Task
import ToString exposing (annotationToString, errorToString, evalErrorDataToString, httpErrorToString, patternToString)
import Types exposing (BackendMsg(..), Cell(..), Code(..), Constant, FileName(..), FrontendModel, FrontendMsg(..), FullCode(..), Function, FunctionName(..), IdDict, Interactives(..), Markdown(..), Output, OutputError(..), OutputValue(..), ParameterName(..), ParsedSection, RawInteractiveValue(..), Section(..), ToBackend(..), ToFrontend(..), TypeName(..), Viewer)
import UI.Source as Source
import Url
import Value


overrideAlwaysHost : Bool
overrideAlwaysHost =
    True



-- MAIN


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , currentFileName = Nothing
      , source = Nothing
      , checksum = Nothing
      , error = ""
      , fileList = []
      , sections = []
      , interactives = Interactives.empty
      , functions = IdDict.empty (\(FunctionName functionName) -> functionName)
      , constants = IdDict.empty (\(FunctionName functionName) -> functionName)
      , viewers = []
      , outputs = IdDict.empty (\(FunctionName functionName) -> functionName)
      , reloadRequests = IdDict.empty (\(FunctionName functionName) -> functionName)
      , runOnHost = True
      }
    , Cmd.batch
        [ Http.get
            { url = "/_x/list/pages/"
            , expect = Http.expectJson GotList (Json.Decode.list Json.Decode.string)
            }
        , sendToBackend RequestStartup
        , notifyIn Poll 4000
        ]
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotText result ->
            case result of
                Ok source ->
                    let
                        ( newModel, newCmd ) =
                            updateCurrentChecksum (evaluateFullSource (FullCode source) model)
                    in
                    ( newModel, Cmd.batch [ newCmd, notifyIn CheckGenerateOutputs 100 ] )

                Err error ->
                    ( { model
                        | error =
                            httpErrorToString error
                      }
                    , Cmd.none
                    )

        GotList result ->
            case result of
                Ok list ->
                    ( { model
                        | fileList =
                            list
                                |> List.filter (String.endsWith ".elm")
                                |> List.filter (\x -> not (String.endsWith "Host.elm" x))
                                |> List.map FileName
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | error =
                            httpErrorToString error
                      }
                    , Cmd.none
                    )

        WroteText _ ->
            ( model, Cmd.none )

        ListItemClicked fileName ->
            ( { model
                | currentFileName = Just fileName
                , outputs = IdDict.clear model.outputs
              }
            , sendToBackend (NewFilenameToBackend fileName)
            )

        InteractiveUpdated names value ->
            let
                newInteractives : Interactives
                newInteractives =
                    Interactives.insert names value model.interactives

                functionName =
                    Tuple.first names

                requestIndex : Int
                requestIndex =
                    IdDict.get functionName model.reloadRequests
                        |> Maybe.withDefault 0
                        |> (\x -> x + 1)
            in
            ( { model
                | interactives = newInteractives
                , reloadRequests = IdDict.insert functionName requestIndex model.reloadRequests
              }
            , Cmd.batch
                [ sendToBackend (InteractivesToBackend newInteractives)
                , notifyIn (ReloadFunction functionName requestIndex) 400
                ]
            )

        CheckGenerateOutputs ->
            case ( IdDict.isEmpty model.functions, Maybe.Extra.isJust model.source, IdDict.isEmpty model.outputs ) of
                ( False, True, True ) ->
                    let
                        newOutputs =
                            IdDict.map
                                (\_ { function, declaration } ->
                                    applyPartiallyApplied model.interactives function declaration
                                )
                                model.functions
                    in
                    ( { model | outputs = newOutputs }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReloadFunction functionName requestIndex ->
            if requestIndex == (IdDict.get functionName model.reloadRequests |> Maybe.withDefault 0) then
                let
                    newOutput : Result OutputError OutputValue
                    newOutput =
                        calculateOutput
                            (if overrideAlwaysHost then
                                True

                             else
                                model.runOnHost
                            )
                            model.interactives
                            model.functions
                            functionName
                in
                ( { model | outputs = IdDict.insert functionName newOutput model.outputs }, Cmd.none )

            else
                ( model, Cmd.none )

        RunOnHostChanged runOnHost ->
            ( { model | runOnHost = runOnHost }, sendToBackend (NewRunOnHostToBackend runOnHost) )

        Poll ->
            let
                checkViewport : Cmd FrontendMsg
                checkViewport =
                    Task.perform (\viewport -> NewScroll viewport.viewport.y) Browser.Dom.getViewport
            in
            ( model, Cmd.batch [ checkViewport, notifyIn Poll 4000 ] )

        NewScroll y ->
            ( model, sendToBackend (NewScrollToBackend y) )

        NoOp ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Startup { interactives, scroll, checksum, fileName, runOnHost } ->
            let
                ( newModel, cmd ) =
                    updateCurrentChecksum
                        { model
                            | interactives = interactives
                            , checksum = Just checksum
                            , currentFileName = fileName
                            , runOnHost = runOnHost
                        }
            in
            ( newModel
            , Cmd.batch
                [ Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 scroll)
                , cmd
                , case fileName of
                    Just currentFileName ->
                        requestPage currentFileName

                    Nothing ->
                        notifyIn CheckGenerateOutputs 100
                ]
            )

        RequestNewSourceToFrontend ->
            case model.source of
                Just source ->
                    ( model
                      --, Cmd.none
                    , Http.post
                        { url = "/_x/write/src/Host.elm"
                        , body = Http.stringBody "application/text" (getHostText source model.functions model.constants)
                        , expect = Http.expectWhatever WroteText
                        }
                    )

                Nothing ->
                    ( { model | error = "Requested new source when there was no source yet" }, Cmd.none )

        RequestNewFileNameToFrontend fileName ->
            ( model
            , requestPage fileName
            )


updateCurrentChecksum : Model -> ( Model, Cmd FrontendMsg )
updateCurrentChecksum model =
    case ( model.source, model.checksum ) of
        ( Just (FullCode source), Just modelChecksum ) ->
            let
                checksum =
                    source |> Hash.fromString |> Hash.toString
            in
            if checksum == modelChecksum then
                ( model, Cmd.none )

            else
                ( { model | checksum = Just checksum }
                , Cmd.batch
                    [ sendToBackend (NewChecksumToBackend checksum)
                    ]
                )

        _ ->
            ( model, Cmd.none )


requestPage : FileName -> Cmd FrontendMsg
requestPage (FileName fileName) =
    Http.get
        { url = "/_x/read/pages/" ++ fileName
        , expect = Http.expectString GotText
        }



-- PARSING TEXT INPUT


parseSections : FullCode -> List ( Code, ParsedSection )
parseSections (FullCode fullSource) =
    let
        newSectionRegex : Regex.Regex
        newSectionRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "\n\n+(?=[^\\s])"
    in
    fullSource
        |> String.replace "\u{000D}\n" "\n"
        |> Regex.split newSectionRegex
        |> List.map (\source -> ( Code source, parseSection (Code source) ))


parseSection : Code -> Result (List DeadEnd) (List Cell)
parseSection (Code source) =
    let
        parser : ParserFast.Parser (List Cell)
        parser =
            ParserFast.skipWhileWhitespaceFollowedBy
                (ParserFast.loopWhileSucceeds
                    (ParserFast.oneOf2
                        (ParserFast.map (\x -> CellDeclaration x.syntax) Elm.Parser.Declarations.declaration)
                        (ParserFast.map (\x -> x |> Node.map Markdown |> CellComment) Elm.Parser.Comments.singleLineComment)
                        |> ParserFast.followedBySkipWhileWhitespace
                    )
                    []
                    (\cell list -> cell :: list)
                    (\list -> List.reverse list)
                )
    in
    ParserFast.run parser source


getHostText : FullCode -> IdDict FunctionName Function -> IdDict FunctionName Constant -> String
getHostText fullSource functions constants =
    let
        maybeFile : Result Error File
        maybeFile =
            makeFile fullSource

        maybeFileName : List String
        maybeFileName =
            maybeFile
                |> Result.map .moduleDefinition
                |> Result.map Node.value
                |> Result.map Module.moduleName
                |> Result.withDefault []

        declarations : List String
        declarations =
            Result.toMaybe maybeFile
                |> Maybe.map
                    (\file ->
                        file
                            |> .declarations
                            |> List.map Node.value
                            |> List.map extractNameFromDeclaration
                    )
                |> Maybe.withDefault []

        viewers : IdDict TypeName Elm.Expression
        viewers =
            declarations
                |> List.filterMap
                    (\name ->
                        if String.startsWith "viewer" name then
                            case IdDict.get (FunctionName name) functions of
                                Just function ->
                                    case function.pairs of
                                        [ ( _, typeName ) ] ->
                                            Just
                                                ( typeName
                                                , Elm.value
                                                    { importFrom = maybeFileName
                                                    , name = name
                                                    , annotation = Nothing
                                                    }
                                                )

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        else
                            Nothing
                    )
                |> IdDict.fromList (\(TypeName typeName) -> typeName)

        fileListDeclaration : Elm.Declaration
        fileListDeclaration =
            (declarations
                |> List.filterMap
                    (\name ->
                        --Elm.fnBuilder
                        --    (\arg1 arg2 ->
                        --        Elm.Op.plus arg1 arg2
                        --    )
                        --    |> Elm.fnArg (Elm.Arg.var "arg1")
                        --    |> Elm.fnArg (Elm.Arg.var "arg2")
                        --    |> Elm.fnDone
                        case ( IdDict.get (FunctionName name) functions, IdDict.get (FunctionName name) constants ) of
                            ( Just function, _ ) ->
                                Just <|
                                    Elm.tuple (Elm.string name)
                                        (Elm.apply
                                            (getCombinerFromNumArgs (List.length function.pairs))
                                            ((function.pairs
                                                |> List.map
                                                    (\pair ->
                                                        Boundary.getSelectorFromTypeName (Tuple.second pair)
                                                    )
                                             )
                                                ++ [ Elm.value
                                                        { importFrom = maybeFileName
                                                        , name = name
                                                        , annotation = Nothing
                                                        }
                                                   , Boundary.getViewerFromTypeName viewers function.return
                                                   ]
                                            )
                                        )

                            ( _, Just (Types.Constant returnValue) ) ->
                                Just <|
                                    Elm.tuple (Elm.string name)
                                        (Elm.apply
                                            (Elm.value
                                                { importFrom = [ "HostFunctions" ]
                                                , name = "constant"
                                                , annotation = Just Elm.Annotation.int -- I need to add this somewhere in order for it to not generate an (incorrect) type signature
                                                }
                                            )
                                            [ Elm.value
                                                { importFrom = maybeFileName
                                                , name = name
                                                , annotation = Nothing
                                                }
                                            , Boundary.getViewerFromTypeName viewers returnValue
                                            ]
                                        )

                            ( _, _ ) ->
                                Nothing
                    )
            )
                |> Elm.list
                |> Elm.declaration "functionList"
    in
    --Elm.file [ "My", "Module" ]
    --    [ Elm.declaration "placeholder"
    --        (Elm.string "a fancy string!")
    --    ]
    Elm.file [ "Host" ] [ fileListDeclaration ]
        |> .contents


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



-- SECTIONS EVALUATION


evaluateFullSource : FullCode -> Model -> Model
evaluateFullSource source model =
    let
        maybeFile : Result Error File
        maybeFile =
            makeFile source

        maybeEnv : Result Error Env
        maybeEnv =
            maybeFile
                |> Result.andThen Eval.Module.buildInitialEnv

        ( viewers, sections, ( functions, constants ) ) =
            evaluateSections
                (if overrideAlwaysHost then
                    True

                 else
                    model.runOnHost
                )
                source
                maybeEnv
    in
    { model
        | source = Just source
        , functions = functions
        , constants = constants
        , viewers = viewers
        , sections = sections
    }


evaluateSections : Bool -> FullCode -> Result Error Env -> ( List Viewer, List Section, ( IdDict FunctionName Types.Function, IdDict FunctionName Types.Constant ) )
evaluateSections runOnHost source maybeEnv =
    let
        viewerSelector : Kernel.InSelector (List (Value -> InterpreterTypes.Config -> Env -> InterpreterTypes.EvalResult (Maybe Kernel.Html.Html))) {}
        viewerSelector =
            Kernel.listIn (Kernel.function Eval.Expression.evalFunction Kernel.anything Kernel.to (Kernel.maybe Kernel.html))

        viewersValue : Result OutputError Value
        viewersValue =
            evaluateName maybeEnv (FunctionName "viewers")

        transformValue : (Value -> InterpreterTypes.Config -> Env -> InterpreterTypes.EvalResult (Maybe Kernel.Html.Html)) -> Viewer
        transformValue func =
            case maybeEnv of
                Err error ->
                    \_ ->
                        error
                            |> errorToString
                            |> OutputError
                            |> Err

                Ok env ->
                    \value ->
                        func value { trace = False } env
                            |> (\( a, _, _ ) -> a)
                            |> Result.mapError evalErrorDataToString
                            |> Result.mapError OutputError
                            |> Result.map (\maybe -> Maybe.map Kernel.Html.htmlToReal maybe)

        viewers : List Viewer
        viewers =
            viewersValue
                |> Result.toMaybe
                |> Maybe.andThen viewerSelector.fromValue
                |> Maybe.withDefault []
                |> List.map transformValue

        viewersError : Maybe OutputError
        viewersError =
            case viewersValue of
                Ok _ ->
                    Nothing

                Err (OutputError error) ->
                    -- This exact error means that the user has not supplied a "viewers" at all
                    -- That's okay, they don't need to! We just want to let them know why their
                    -- viewers are funky if they do expect them to show up
                    if String.startsWith "NameError: " error && String.endsWith "viewers" error then
                        Nothing

                    else
                        Just (OutputError error)

        ( evaluatedSections, functions, constants ) =
            parseSections source
                |> List.foldr
                    (\item ( list, currentFunctions, currentConstants ) ->
                        let
                            ( value, maybeFunction, maybeConstant ) =
                                evaluateSection runOnHost maybeEnv item

                            newFunctions : IdDict FunctionName Function
                            newFunctions =
                                case maybeFunction of
                                    Just ( name, function ) ->
                                        IdDict.insert name function currentFunctions

                                    Nothing ->
                                        currentFunctions

                            newConstants : IdDict FunctionName Constant
                            newConstants =
                                case maybeConstant of
                                    Just ( name, constant ) ->
                                        IdDict.insert name constant currentConstants

                                    Nothing ->
                                        currentConstants
                        in
                        ( value :: list, newFunctions, newConstants )
                    )
                    ( []
                    , IdDict.empty (\(FunctionName functionName) -> functionName)
                    , IdDict.empty (\(FunctionName functionName) -> functionName)
                    )
    in
    case viewersError of
        Just error ->
            ( viewers, ErrorSection [ OutputError "\"viewers\" failed to evaluate:", error ] :: evaluatedSections, ( functions, constants ) )

        Nothing ->
            ( viewers, evaluatedSections, ( functions, constants ) )


evaluateSection : Bool -> Result Error Env -> ( Code, Result error (List Cell) ) -> ( Section, Maybe ( FunctionName, Function ), Maybe ( FunctionName, Constant ) )
evaluateSection runOnHost maybeEnv ( source, parsedSection ) =
    case parsedSection of
        Ok cells ->
            let
                lastCode : Maybe Cell
                lastCode =
                    List.filter isCode cells
                        |> List.Extra.last

                isCode : Cell -> Bool
                isCode cell =
                    case cell of
                        CellDeclaration _ ->
                            True

                        _ ->
                            False

                isComment : Cell -> Bool
                isComment cell =
                    case cell of
                        CellComment _ ->
                            True

                        _ ->
                            False
            in
            case lastCode of
                Just (CellDeclaration (Node _ declaration)) ->
                    let
                        patterns2 =
                            extractPatternFromDeclaration declaration

                        typeAnnotations : List TypeAnnotation
                        typeAnnotations =
                            declarationTypeAnnotation declaration |> Result.map Boundary.bigAnnotationToList |> Result.withDefault []

                        --maybePairs2 =
                        --    Boundary.parseTogether (patterns2 |> List.map Node.value) declaration List.length
                        --
                        --a =
                        --    Boundary.parseTogether
                        functionNameString : String
                        functionNameString =
                            extractNameFromDeclaration declaration

                        functionName : FunctionName
                        functionName =
                            functionNameString |> FunctionName
                    in
                    if (List.length typeAnnotations <= 1) && runOnHost then
                        ( EvaluatedSection source
                            (case Dict.get functionNameString hostFunctionList of
                                Just hostFunction ->
                                    hostFunction []

                                Nothing ->
                                    Err (OutputError "Host functions not yet written to file, because this constant was not found.")
                            )
                        , Nothing
                        , makeConstant functionName declaration
                        )

                    else
                        case evaluateName maybeEnv functionName of
                            Err error ->
                                ( EvaluatedSection source (Err error), Nothing, Nothing )

                            Ok (PartiallyApplied ((PartiallyAppliedFunction _ alreadyApplied patterns _ _) as function)) ->
                                let
                                    maybePairs : Result OutputError ( List ( ParameterName, TypeName ), TypeName )
                                    maybePairs =
                                        Boundary.parseTogether (patterns |> List.map Node.value) declaration (List.length alreadyApplied)
                                in
                                case maybePairs of
                                    Err error ->
                                        ( EvaluatedSection source (Err error), Nothing, Nothing )

                                    Ok ( pairs, return ) ->
                                        ( InteractiveSection source functionName
                                        , Just
                                            ( functionName
                                            , { function = function
                                              , declaration = declaration
                                              , pairs = pairs
                                              , return = return
                                              }
                                            )
                                        , Nothing
                                        )

                            Ok value ->
                                ( EvaluatedSection source (value |> OutputValue |> Ok)
                                , Nothing
                                , makeConstant functionName declaration
                                )

                _ ->
                    ( List.filter isComment cells
                        |> List.map
                            (\comment ->
                                case comment of
                                    CellComment (Node _ (Markdown text)) ->
                                        let
                                            contents =
                                                String.dropLeft 2 text
                                        in
                                        if String.startsWith " " contents then
                                            String.dropLeft 1 contents

                                        else
                                            contents

                                    _ ->
                                        ""
                            )
                        |> String.join "\n"
                        |> Markdown
                        |> MarkdownSection
                    , Nothing
                    , Nothing
                    )

        Err _ ->
            ( CodeSection source, Nothing, Nothing )


makeConstant functionName declaration =
    Boundary.declarationTypeAnnotation declaration
        |> Result.map Boundary.bigAnnotationToList
        |> Result.andThen Boundary.parseReturnType
        |> Result.toMaybe
        |> Maybe.map (\typeName -> ( functionName, Types.Constant typeName ))


makeFile : FullCode -> Result Error File
makeFile (FullCode source) =
    let
        file : Result (List DeadEnd) File
        file =
            Elm.Parser.parseToFile source
    in
    Result.mapError ParsingError file



-- EVALUATION


hostFunctionList : Dict String (List Value -> Output)
hostFunctionList =
    Host.functionList
        |> Dict.fromList


calculateOutput : Bool -> Interactives -> IdDict FunctionName Function -> FunctionName -> Output
calculateOutput runOnHost interactives functions ((FunctionName functionNameString) as functionName) =
    case IdDict.get functionName functions of
        Just { function, declaration } ->
            if not runOnHost then
                applyPartiallyApplied interactives function declaration

            else
                case Dict.get functionNameString hostFunctionList of
                    Just hostFunction ->
                        applyPartiallyAppliedHost interactives function hostFunction functionName declaration

                    Nothing ->
                        OutputError "Couldn't load function on host" |> Err

        Nothing ->
            OutputError "No function stored with this name! This is an internal error." |> Err


applyPartiallyApplied : Interactives -> PartiallyAppliedFunction -> Declaration -> Output
applyPartiallyApplied evalInteractives partiallyApplied declaration =
    let
        (PartiallyAppliedFunction baseEnv alreadyApplied patterns _ expression) =
            partiallyApplied

        maybePairs : Result OutputError ( List ( ParameterName, TypeName ), TypeName )
        maybePairs =
            Boundary.parseTogether (patterns |> List.map Node.value) declaration (List.length alreadyApplied)

        maybeValuePairs : Result OutputError (List ( ParameterName, Value ))
        maybeValuePairs =
            Boundary.parseValuesTogether (patterns |> List.map Node.value) alreadyApplied

        functionName : FunctionName
        functionName =
            extractNameFromDeclaration declaration |> FunctionName

        insertIntoEnvFromValue : ( ParameterName, Value ) -> Dict String Value -> Dict String Value
        insertIntoEnvFromValue ( ParameterName binding, value ) localValues =
            Dict.insert binding value localValues

        insertIntoEnvFromType : ( ParameterName, TypeName ) -> Dict String Value -> Result OutputError (Dict String Value)
        insertIntoEnvFromType ( ParameterName binding, TypeName typeName ) localValues =
            let
                maybeTypeNode : Maybe InteractiveElement
                maybeTypeNode =
                    Dict.get typeName Boundary.typeNodeMap

                maybeRawValue : Maybe RawInteractiveValue
                maybeRawValue =
                    Interactives.get ( functionName, ParameterName binding ) evalInteractives

                typeNodeToValue : InteractiveElement -> RawInteractiveValue -> Result OutputError Value
                typeNodeToValue typeNode rawValue =
                    typeNode.conversion rawValue

                maybeValue : Maybe (Result OutputError Value)
                maybeValue =
                    Maybe.map2 typeNodeToValue maybeTypeNode maybeRawValue
            in
            case maybeValue of
                Just (Ok value) ->
                    Dict.insert binding value localValues |> Ok

                _ ->
                    ("Missing \"" ++ binding ++ "\"") |> OutputError |> Err

        updateEnvFromValues : Env -> Env
        updateEnvFromValues env =
            case maybeValuePairs of
                Err _ ->
                    env

                Ok pairs ->
                    { env
                        | values =
                            List.foldl
                                insertIntoEnvFromValue
                                env.values
                                pairs
                    }

        updateEnvFromType : Env -> Result OutputError Env
        updateEnvFromType env =
            case maybePairs of
                Err _ ->
                    Ok env

                Ok pairs ->
                    case
                        Result.Extra.foldlWhileOk
                            insertIntoEnvFromType
                            env.values
                            (Tuple.first pairs)
                    of
                        Ok newValues ->
                            { env
                                | values = newValues
                            }
                                |> Ok

                        Err err ->
                            Err err

        functionOutput : Result OutputError Value
        functionOutput =
            (baseEnv |> updateEnvFromValues |> updateEnvFromType)
                |> Result.andThen (\env -> evaluate (Ok env) expression)
    in
    Result.map OutputValue functionOutput


applyPartiallyAppliedHost : Interactives -> PartiallyAppliedFunction -> (List Value -> Output) -> FunctionName -> Declaration -> Output
applyPartiallyAppliedHost interactives function hostFunction functionName declaration =
    let
        --patterns : List (Node Pattern)
        --patterns =
        --    extractPatternFromDeclaration declaration
        (PartiallyAppliedFunction _ alreadyApplied patterns _ _) =
            function

        maybePairs : Result OutputError ( List ( ParameterName, TypeName ), TypeName )
        maybePairs =
            Boundary.parseTogether (patterns |> List.map Node.value) declaration (List.length alreadyApplied)

        find : ( ParameterName, TypeName ) -> Result OutputError Value
        find ( parameterName, TypeName typeName ) =
            let
                maybeTypeNode : Result OutputError InteractiveElement
                maybeTypeNode =
                    Dict.get typeName Boundary.typeNodeMap |> Result.fromMaybe (OutputError "No matching type node")

                maybeRawValue : Result OutputError RawInteractiveValue
                maybeRawValue =
                    Interactives.get ( functionName, parameterName ) interactives |> Result.fromMaybe (OutputError "No rawvalue in interactives")
            in
            Result.map2 Tuple.pair maybeTypeNode maybeRawValue
                |> Result.andThen (\( typeNode, rawValue ) -> typeNode.conversion rawValue)

        parameters : Result OutputError (List Value)
        parameters =
            maybePairs |> Result.andThen (\( list, return ) -> Result.Extra.combineMap find list)

        --(baseEnv |> updateEnvFromValues |> updateEnvFromType)
        --    |> Result.andThen (\env -> evaluate (Ok env) expression)
    in
    Result.andThen hostFunction parameters


evaluateName : Result Error Env -> FunctionName -> Result OutputError Value
evaluateName maybeEnv (FunctionName expressionName) =
    let
        expression : Expression
        expression =
            Elm.Syntax.Expression.FunctionOrValue
                []
                expressionName

        expressionNode : Node Expression
        expressionNode =
            Node.empty expression
    in
    evaluate maybeEnv expressionNode


evaluate : Result Error Env -> Node Expression -> Result OutputError Value
evaluate maybeEnv expressionNode =
    let
        moduleRun : Result Error Value
        moduleRun =
            case maybeEnv of
                Err e ->
                    Err e

                Ok env ->
                    let
                        ( result, _, _ ) =
                            Eval.Expression.evalExpression
                                expressionNode
                                { trace = False }
                                env
                    in
                    Result.mapError InterpreterTypes.EvalError result
    in
    moduleRun |> Result.mapError errorToString |> Result.mapError OutputError



-- UTILITIES


extractNameFromDeclaration : Declaration -> String
extractNameFromDeclaration declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .declaration
                |> Node.value
                |> .name
                |> Node.value

        AliasDeclaration _ ->
            "typealias"

        CustomTypeDeclaration _ ->
            "customtype"

        PortDeclaration _ ->
            "portdeclaration"

        InfixDeclaration _ ->
            "infixdeclaration"

        Destructuring _ _ ->
            "destructuring"


extractPatternFromDeclaration : Declaration -> List (Node Pattern)
extractPatternFromDeclaration declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .declaration
                |> Node.value
                |> .arguments

        _ ->
            []



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (Element.column
                -- left could be 185
                [ Element.alignLeft, Element.paddingEach { top = 40, left = 40, right = 40, bottom = 0 } ]
                (Element.el
                    [ Element.centerX

                    --, Font.typeface "IBM Plex Sans"
                    , Font.family
                        [ Font.typeface "Quicksand"
                        , Font.sansSerif
                        ]

                    --, Font.typeface [ "IBM Plex Sans", "sans-serif" ]
                    --, Font.family
                    --    [ Font.external
                    --        { name = "IBM Plex Sans"
                    --        , url = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,100..700;1,100..700&display=swap"
                    --        }
                    --    , Font.sansSerif
                    --    ]
                    , Font.size 72
                    , Font.color (Element.rgb255 120 120 120)
                    , Element.paddingEach { defaultPadding | bottom = 20 }
                    ]
                    (Element.text "elm-clerk")
                    :: viewError model.error
                    ++ (Element.column [ Element.spacing 4 ] (model.fileList |> List.map viewFileListItem)
                            |> Element.el [ Element.paddingXY 0 4 ]
                            |> List.singleton
                       )
                    ++ (if overrideAlwaysHost then
                            []

                        else
                            viewSettings model.runOnHost
                       )
                    --++ (model.source
                    --        |> Maybe.map List.singleton
                    --        |> Maybe.withDefault []
                    --        |> List.map (\source -> viewCode (Code (getHostText source model.functions model.constants)))
                    --   )
                    ++ (case ( model.source, model.currentFileName, model.error ) of
                            ( Just _, _, _ ) ->
                                model.sections
                                    |> List.map
                                        (viewSection model.viewers
                                            model.outputs
                                            model.functions
                                            model.interactives
                                        )

                            ( _, Nothing, _ ) ->
                                [ Element.el
                                    [ Font.family
                                        [ Font.typeface "Quicksand"
                                        , Font.sansSerif
                                        ]
                                    ]
                                    (Element.text
                                        (if List.length model.fileList >= 0 then
                                            "No file selected"

                                         else
                                            "No .elm files in the pages folder. Create one!"
                                        )
                                    )
                                ]

                            ( _, _, "" ) ->
                                [ Element.el
                                    [ Font.family
                                        [ Font.typeface "Quicksand"
                                        , Font.sansSerif
                                        ]
                                    ]
                                    (Element.text "Loading...")
                                ]

                            _ ->
                                []
                       )
                )
            )
        ]
    }


viewFileListItem : FileName -> Element FrontendMsg
viewFileListItem (FileName listItem) =
    Element.Input.button []
        { onPress = Just (ListItemClicked (FileName listItem))
        , label = Element.text listItem
        }


viewSettings : Bool -> List (Element FrontendMsg)
viewSettings runOnHost =
    Element.Input.checkbox []
        { label = Element.Input.labelRight [] (Element.text "Run on host")
        , checked = runOnHost
        , onChange = RunOnHostChanged
        , icon = Element.Input.defaultCheckbox
        }
        |> Element.el [ Element.Border.solid, Element.Border.width 1, Element.padding 9 ]
        |> Element.el [ Element.paddingXY 0 4 ]
        |> List.singleton



--= Label LabelLocation (List (Attribute msg)) (Element msg)
--{ onChange : Bool -> msg
--, icon : Bool -> Element msg
--, checked : Bool
--, label : Label msg


viewSection : List Viewer -> IdDict FunctionName Output -> IdDict FunctionName Function -> Interactives -> Section -> Element FrontendMsg
viewSection viewers outputs functions interactives section =
    let
        applyViewer : Value -> Result OutputError (Maybe (Html.Html FrontendMsg))
        applyViewer value =
            let
                encoded =
                    Kernel.encodedValue.toValue value
            in
            List.Extra.stoppableFoldl
                (\viewer _ ->
                    case viewer encoded of
                        Ok Nothing ->
                            Continue (Ok Nothing)

                        Ok (Just transformed) ->
                            Stop (Ok (Just transformed))

                        Err error ->
                            Stop (Err error)
                )
                (Ok Nothing)
                viewers

        transform : Result OutputError OutputValue -> Result OutputError OutputValue
        transform valueResult =
            case valueResult of
                Err _ ->
                    valueResult

                Ok (OutputHtml _) ->
                    valueResult

                Ok (OutputValue value) ->
                    case applyViewer value of
                        Err error ->
                            Err error

                        Ok (Just html) ->
                            Ok (OutputHtml html)

                        Ok Nothing ->
                            Ok (OutputValue value)
    in
    Element.column [ width fill, paddingEach { top = 10, right = 0, bottom = 0, left = 0 } ]
        (case section of
            MarkdownSection markdown ->
                [ viewMarkdownHtml markdown ]

            CodeSection code ->
                [ viewCode code ]

            EvaluatedSection code output ->
                [ viewCode code
                , case transform output of
                    Ok value ->
                        viewOutputValue value

                    Err value ->
                        viewOutputError value
                ]

            InteractiveSection code functionName ->
                [ viewCode code ]
                    ++ (case Maybe.map2 Tuple.pair (IdDict.get functionName outputs) (IdDict.get functionName functions) of
                            Nothing ->
                                [ viewOutputError (OutputError "Evaluating...") ]

                            Just ( output, function ) ->
                                [ Element.row
                                    [ width fill
                                    , Element.Background.color (Element.rgb255 240 240 240)
                                    , Element.paddingXY (graySidePadding - 9) 0
                                    ]
                                    (List.map
                                        (viewInteractive interactives functionName)
                                        function.pairs
                                    )
                                , case transform output of
                                    Ok value ->
                                        viewOutputValue value

                                    Err value ->
                                        viewOutputError value
                                ]
                       )

            ErrorSection error ->
                List.map viewOutputError error
        )


viewInteractive : Interactives -> FunctionName -> ( ParameterName, TypeName ) -> Element FrontendMsg
viewInteractive interactives functionName ( (ParameterName bindingString) as binding, TypeName typeName ) =
    let
        maybeValue : Maybe RawInteractiveValue
        maybeValue =
            Interactives.get ( functionName, binding ) interactives
    in
    case Dict.get typeName Boundary.typeNodeMap of
        Nothing ->
            viewOutputError (OutputError (bindingString ++ " - Interactive input of \"" ++ typeName ++ "\" not supported"))

        Just interactiveElement ->
            Element.Lazy.lazy3 interactiveElement.element functionName binding maybeValue


viewMarkdownHtml : Markdown -> Element FrontendMsg
viewMarkdownHtml =
    Element.Lazy.lazy
        (\markdown ->
            let
                markdownView : Markdown -> Result String (List (Html.Html msg))
                markdownView (Markdown localMarkdown) =
                    localMarkdown
                        |> Markdown.Parser.parse
                        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
                        |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
            in
            case markdownView markdown of
                Ok values ->
                    values
                        |> Html.div []
                        |> Element.html
                        |> Element.el
                            [ width maxWidth
                            , Font.family
                                [ Font.typeface "Fira Sans"
                                , Font.sansSerif
                                ]
                            ]

                Err err ->
                    Element.text err
        )


viewMarkdown : Markdown -> Element FrontendMsg
viewMarkdown markdown =
    let
        markdownView : Markdown -> Result String (List (Element msg))
        markdownView (Markdown localMarkdown) =
            localMarkdown
                |> Markdown.Parser.parse
                |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
                |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.ElmUi.renderer)
    in
    case markdownView markdown of
        Ok values ->
            values
                |> Element.column []

        Err err ->
            Element.text err


viewChart : Element msg
viewChart =
    Element.el [ width maxWidth, Element.paddingEach { top = 40, left = 40, right = 40, bottom = 0 } ] <|
        Element.html <|
            C.chart
                [ CA.height 200
                , CA.width 300
                , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
                ]
                [ C.xLabels []
                , C.yLabels [ CA.withGrid ]
                , C.series .x
                    [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
                    , C.interpolated .z [ CA.monotone ] [ CA.square ]
                    ]
                    [ { x = 1, y = 2, z = 3 }
                    , { x = 5, y = 4, z = 1 }
                    , { x = 10, y = 2, z = 4 }
                    ]
                ]


viewCode : Code -> Element msg
viewCode =
    Element.Lazy.lazy
        (\(Code code) ->
            Element.el [ width maxWidth ] <|
                Source.viewExpression [ scrollbarX, monospace ]
                    { highlight = Nothing
                    , buttons = []
                    , source = code
                    }
        )


viewError : String -> List (Element FrontendMsg)
viewError error =
    case error of
        "" ->
            []

        _ ->
            String.split "\n" error |> List.map OutputError |> List.map viewOutputError


viewOutputValue : OutputValue -> Element FrontendMsg
viewOutputValue =
    Element.Lazy.lazy
        (\outputValue ->
            let
                viewHtml : Html.Html FrontendMsg -> Element FrontendMsg
                viewHtml html =
                    html
                        |> Element.html
                        |> Element.el [ Element.paddingEach { top = 12, right = graySidePadding, bottom = 4, left = graySidePadding } ]
            in
            case outputValue of
                OutputHtml html ->
                    viewHtml html

                OutputValue value ->
                    case Kernel.html.fromValue value of
                        Just html ->
                            html
                                |> Kernel.Html.htmlToReal
                                |> viewHtml

                        _ ->
                            viewOutput ("-> " ++ Value.toString value)
        )


viewOutputError : OutputError -> Element FrontendMsg
viewOutputError =
    Element.Lazy.lazy
        (\(OutputError output) ->
            viewOutput output
        )


viewOutput : String -> Element msg
viewOutput output =
    Element.el [ Element.paddingXY 0 3, width fill ] <|
        Element.el
            [ width <| maxWidth
            , height <| fill
            , Element.Background.color (Element.rgb255 240 240 240)
            , Element.paddingXY graySidePadding 8
            ]
        <|
            el
                [ width shrink
                , height <| maximum 300 shrink
                , scrollbarY
                , clipX
                , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                ]
                (paragraph [ monospace, Font.size 20 ] [ text output ])


defaultPadding : { top : Int, right : Int, bottom : Int, left : Int }
defaultPadding =
    { top = 0, right = 0, bottom = 0, left = 0 }


maxWidth : Element.Length
maxWidth =
    maximum 1000 fill


graySidePadding : Int
graySidePadding =
    14


monospace : Attribute msg
monospace =
    Font.family
        [ Font.typeface "Fira Code"
        , Font.monospace
        ]
