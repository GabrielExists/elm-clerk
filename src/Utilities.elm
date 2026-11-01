module Utilities exposing (..)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))



-- UTILITIES


extractNameFromDeclarationNode : Declaration -> Maybe (Node String)
extractNameFromDeclarationNode declaration =
    case declaration of
        FunctionDeclaration function ->
            function
                |> .declaration
                |> Node.value
                |> .name
                |> Just

        _ ->
            Nothing


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
