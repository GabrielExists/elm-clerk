module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Http
import IntTypes


type alias FrontendModel =
    { key : Key
    , message : String
    , source : Maybe String
    , output : Maybe Output
    }


type alias BackendModel =
    { message : String
    , source : Maybe String
    , output : Maybe Output
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotText (Result Http.Error String)


type ToBackend
    = NoOpToBackend
    | OutputToBackend String Output


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type alias Output = Result IntTypes.Error IntTypes.Value
