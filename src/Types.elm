module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Elm.Dependency
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Node exposing (Node)
import Elm.TypeInference
import Elm.Version
import FastDict as Dict exposing (Dict)
import Html exposing (Html)
import Http
import InterpreterTypes exposing (PartiallyAppliedFunction, Value)
import Lamdera exposing (ClientId)
import Parser exposing (DeadEnd)
import Task exposing (Task)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , currentFileName : Maybe FileName
    , source : Maybe FullCode
    , checksum : Maybe Checksum
    , error : String
    , fileList : List FileName
    , sections : List Section
    , interactives : Interactives
    , reloadRequests : IdDict FunctionName Int
    , functions : IdDict FunctionName Function
    , constants : IdDict FunctionName Constant
    , viewers : List Viewer
    , outputs : IdDict FunctionName Output
    , runOnHost : Bool
    , depMainProject : Maybe Elm.Project.ApplicationInfo
    , dependencies : List DependencyBundle
    , depTotalDependencies : Int
    , depInferenceProject : Maybe Elm.TypeInference.Project
    }


type Constant
    = Constant TypeName


type alias BackendModel =
    { message : String
    , interactives : Interactives
    , scroll : Float
    , checksum : Checksum
    , fileName : Maybe FileName
    , runOnHost : Bool
    }


type alias Function =
    { function : PartiallyAppliedFunction
    , declaration : Declaration
    , pairs : List ( ParameterName, TypeName )
    , return : TypeName
    }


type alias DependencyName =
    ( Elm.Package.Name, Elm.Version.Version )


type alias DependencyBundle =
    { name : DependencyName, project : Elm.Project.Project, docs : List Elm.Docs.Module }


type alias Selection =
    {}


type alias Output =
    Result OutputError OutputValue


type alias ParsedSection =
    Result (List DeadEnd) (List Cell)


type alias Viewer =
    Value -> Result OutputError (Maybe (Html.Html FrontendMsg))


type Cell
    = CellComment (Node Markdown)
    | CellDeclaration (Node Declaration)


{-| Dictionary of (FunctionName,ParameterName) to user-provided value for that param
-}
type Interactives
    = Interactives (Dict ( String, String ) RawInteractiveValue)


type IdDict kind value
    = IdDict (Dict String value) (kind -> String)



-- Various types of string


{-| To be rendered as markdown
-}
type Markdown
    = Markdown String


{-| The full code of a module
-}
type FullCode
    = FullCode String


{-| A snippet of code
-}
type Code
    = Code String


{-| The name of an interactive function
-}
type FunctionName
    = FunctionName String


{-| The name of a parameter of an interactive function
-}
type ParameterName
    = ParameterName String


{-| What the user puts into a field underneath a function, before validation or parsing
-}
type RawInteractiveValue
    = RawInteractiveValue String


{-| Marked for being output in a monospace section
-}
type OutputError
    = OutputError String


{-| The result of an interactive function
-}
type OutputValue
    = OutputValue Value
    | OutputHtml (Html.Html FrontendMsg)


{-| A type name from parsing type annotations
-}
type TypeName
    = TypeName String


{-| The name of a file in the pages folder, including the .elm extension
-}
type FileName
    = FileName String


{-| The checksum of the contents of a page
-}
type Checksum
    = Checksum String



-- Messages


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotList (Result Http.Error (List String))
    | Open1FileName FileName
    | Open2FetchedFile (Result Http.Error String)
    | Open6FetchedElmJson (Result Http.Error Elm.Project.Project)
    | Open7FetchedDependency (Result Http.Error DependencyBundle)
    | Open8FetchedDefaults (Result Http.Error String)
    | Open9WroteHost (Result Http.Error ())
    | ForceRefreshHost
    | InteractiveUpdated ( FunctionName, ParameterName ) RawInteractiveValue
    | CheckGenerateOutputs
    | ReloadFunction FunctionName Int
    | RunOnHostChanged Bool
    | PollScroll
    | Refresh1Poll
    | Refresh2FetchedFile (Result Http.Error String)
    | NewScroll Float
    | NoOp


type ToBackend
    = NoOpToBackend
    | InteractivesToBackend Interactives
    | NewScrollToBackend Float
    | NewRunOnHostToBackend Bool
    | RequestStartup
    | Open3FilenameChecksumToBackend FileName Checksum


type BackendMsg
    = NoOpBackendMsg
    | Open4RequestLoad ClientId FileName


type ToFrontend
    = NoOpToFrontend
    | Startup { interactives : Interactives, scroll : Float, checksum : Checksum, fileName : Maybe FileName, runOnHost : Bool }
    | Open5RequestLoadToFrontend


type Section
    = MarkdownSection Markdown
    | CodeSection Code
    | EvaluatedSection Code (Result OutputError OutputValue)
    | InteractiveSection Code FunctionName
    | ErrorSection (List OutputError)
