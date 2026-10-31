module Backend exposing (Model, app)

import Common exposing (notifyIn)
import FastDict as Dict
import Interactives
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Types exposing (BackendModel, BackendMsg(..), FileName(..), FrontendMsg(..), ToBackend(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , interactives = Interactives.empty
      , scroll = 0
      , checksum = ""
      , fileName = Nothing
      , runOnHost = False
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Open6RequestHost clientId ->
            ( model, sendToFrontend clientId Open7RequestHostToFrontend )

        Open2RequestLoad clientId fileName ->
            ( model, sendToFrontend clientId (Open3RequestLoadToFrontend fileName) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        InteractivesToBackend interactives ->
            ( { model | interactives = interactives }, Cmd.none )

        RequestStartup ->
            ( model
            , sendToFrontend clientId
                (Startup
                    { interactives = model.interactives
                    , scroll = model.scroll
                    , checksum = model.checksum
                    , fileName = model.fileName
                    , runOnHost = model.runOnHost
                    }
                )
            )

        NewScrollToBackend y ->
            ( { model | scroll = y }, Cmd.none )

        NewRunOnHostToBackend runOnHost ->
            ( { model | runOnHost = runOnHost }, Cmd.none )

        Open5ChecksumToBackend newChecksum ->
            ( { model | checksum = newChecksum }, notifyIn (Open6RequestHost clientId) 1000 )

        Open1FilenameToBackend fileName ->
            ( { model | fileName = Just fileName }, notifyIn (Open2RequestLoad clientId fileName) 1000 )
