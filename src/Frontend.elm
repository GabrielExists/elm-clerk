module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Elm.Parser
import Elm.Syntax.Expression
import Elm.Syntax.File exposing (File)
import Eval
import Eval.Expression as EEval
import Eval.Module as MEval
import Html
import Html.Attributes as Attr
import Http
import IntTypes exposing (Error, Value)
import Lamdera exposing (sendToBackend)
import Parser exposing (DeadEnd)
import Types exposing (..)
import UI.Source as Source
import Url
import Value


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding! "
      , sources = []
      , outputs = []
      }
    , Http.get
        { url = "/_x/read/pages/Page1.elm"
        , expect = Http.expectString GotText
        }
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

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotText result ->
            case result of
                Ok fullText ->
                    let
                        sources : Sources
                        sources =
                            [ fullText, fullText ]

                        module_run source =
                            MEval.eval source
                                (Elm.Syntax.Expression.FunctionOrValue
                                    []
                                    "output"
                                )

                        outputs : Outputs
                        outputs =
                            [ [ module_run fullText |> module_run_to_string ]
                            , runCustom fullText
                            ]
                    in
                    ( { model | sources = sources, outputs = outputs }
                    , sendToBackend (OutputToBackend sources outputs)
                    )

                Err error ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


parse : String -> List String
parse source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            [ "Ok" ]

        Err deadEnds ->
            deadEndsToString deadEnds


deadEndsToString : List DeadEnd -> List String
deadEndsToString deadEnds =
    deadEnds
        |> List.map
            (\deadEnd ->
                "At row "
                    ++ String.fromInt deadEnd.row
                    ++ ", column "
                    ++ String.fromInt deadEnd.col
                    ++ ", problem : "
                    ++ (case deadEnd.problem of
                            Parser.Expecting string ->
                                "Expecting " ++ string

                            Parser.ExpectingInt ->
                                "Expecting Int"

                            Parser.ExpectingHex ->
                                "Expecting hex"

                            Parser.ExpectingOctal ->
                                "Expecting Octal"

                            Parser.ExpectingBinary ->
                                "Expecting Binary"

                            Parser.ExpectingFloat ->
                                "Expecting Float"

                            Parser.ExpectingNumber ->
                                "Expecting Number"

                            Parser.ExpectingVariable ->
                                "Expecting Variable"

                            Parser.ExpectingSymbol string ->
                                "Expecting symbol " ++ string

                            Parser.ExpectingKeyword string ->
                                "Expecting keyword " ++ string

                            Parser.ExpectingEnd ->
                                "Expecting end"

                            Parser.UnexpectedChar ->
                                "Unexpected char"

                            Parser.Problem string ->
                                "Problem: " ++ string

                            Parser.BadRepeat ->
                                "BadRepeat"
                       )
            )


runCustom : String -> List String
runCustom source =
    [ "OUTPUT", "TWO" ]


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            ([ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
             , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text model.message ]
             ]
                ++ List.map2 viewSection model.sources model.outputs
            )
        ]
    }


viewSection : String -> List String -> Html.Html FrontendMsg
viewSection source output =
    Html.div []
        ([ Html.div
            [ Attr.style "font-family" "sans-serif"
            , Attr.style "padding-top" "40px"
            ]
            [ Element.layout []
                (Source.view []
                    { highlight = Nothing
                    , buttons = []
                    , source = source
                    }
                )
            ]
         ]
            ++ List.map viewOutput output
        )


viewOutput : String -> Html.Html msg
viewOutput output =
    Html.div
        [ Attr.style "font-family" "monospace"
        , Attr.style "font-size" "40px"
        , Attr.style "padding-top" "40px"
        ]
        [ Html.text output ]


module_run_to_string : Result Error Value -> String
module_run_to_string output =
    case output of
        Ok value ->
            Value.toString value

        Err _ ->
            "Error"
