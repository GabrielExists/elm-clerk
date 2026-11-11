module Backend exposing (Model, app)

import Cmd.Extra exposing (pure)
import Common exposing (notifyIn)
import FastDict as Dict
import Interactives
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Types exposing (BackendModel, BackendMsg(..), Checksum(..), FileName(..), FrontendMsg(..), ToBackend(..), ToFrontend(..))


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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        NoOpToBackend ->
            pure model

        InteractivesToBackend interactives ->
            pure { model | interactives = interactives }

        Startup1RequestStored ->
            ( model
            , sendToFrontend clientId
                (Startup2StoredToFrontend
                    { interactives = model.interactives
                    , scroll = model.scroll
                    , fileName = model.fileName
                    , runOnHost = model.runOnHost
                    }
                )
            )

        NewScrollToBackend y ->
            pure { model | scroll = y }

        NewRunOnHostToBackend runOnHost ->
            pure { model | runOnHost = runOnHost }

        FilenameToBackend fileName ->
            pure { model | fileName = Just fileName }
