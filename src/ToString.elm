module ToString exposing (..)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import IntTypes exposing (Value(..))
import Parser exposing (DeadEnd)
import Value


listToString : String -> List String -> String
listToString name list =
    name ++ " [" ++ String.join ", " list ++ "]"


listToStringParen : String -> List String -> String
listToStringParen name list =
    name ++ " (" ++ String.join ", " list ++ ")"


qualifiedNameRefToString : QualifiedNameRef -> String
qualifiedNameRefToString name =
    name.moduleName
        ++ [ name.name ]
        |> String.join "."


functionDeclarationToString : Value -> String
functionDeclarationToString value =
    case value of
        PartiallyApplied env values patterns maybeName (Node _ expression) ->
            [ "  Already applied:" ]
                ++ (values
                        |> List.map Value.toString
                   )
                ++ [ "  Pattern:" ]
                ++ (patterns
                        |> List.map Node.value
                        |> List.map patternToString
                   )
                ++ [ "  Name:" ]
                ++ (case maybeName of
                        Just name ->
                            name.moduleName
                                ++ [ name.name ]
                                |> String.join "."
                                |> List.singleton

                        Nothing ->
                            [ "" ]
                   )
                ++ [ " Expression:" ]
                ++ [ expressionToString expression ]
                |> String.join "\n"

        _ ->
            ""


evalErrorKindToString : IntTypes.EvalErrorKind -> String
evalErrorKindToString errorKind =
    case errorKind of
        IntTypes.TypeError string ->
            "TypeError: " ++ string

        IntTypes.Unsupported string ->
            "Unsupported: " ++ string

        IntTypes.NameError string ->
            "NameError: " ++ string

        IntTypes.Todo string ->
            "Todo: " ++ string


deadEndsToStrings : List DeadEnd -> List String
deadEndsToStrings deadEnds =
    deadEnds |> List.map deadEndToString


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
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


patternToString pattern =
    case pattern of
        AllPattern ->
            "AllPattern"

        UnitPattern ->
            "UnitPattern"

        CharPattern char ->
            "CharPattern " ++ String.fromChar char

        StringPattern string ->
            "StringPattern " ++ string

        IntPattern int ->
            "IntPattern " ++ String.fromInt int

        HexPattern hex ->
            "HexPattern " ++ String.fromInt hex

        FloatPattern float ->
            "FloatPattern " ++ String.fromFloat float

        TuplePattern patterns ->
            patterns
                |> List.map Node.value
                |> List.map patternToString
                |> listToStringParen "TuplePattern"

        RecordPattern record ->
            record
                |> List.map Node.value
                |> listToString "RecordPattern"

        UnConsPattern first second ->
            "UnConsPattern"
                ++ (first
                        |> Node.value
                        |> patternToString
                   )
                ++ " "
                ++ (second
                        |> Node.value
                        |> patternToString
                   )

        ListPattern list ->
            list
                |> List.map Node.value
                |> List.map patternToString
                |> listToStringParen "ListPattern"

        VarPattern name ->
            "VarPattern: " ++ name

        NamedPattern name list ->
            "NamedPattern"
                ++ qualifiedNameRefToString name
                ++ (list
                        |> List.map Node.value
                        |> List.map patternToString
                        |> listToStringParen ""
                   )

        AsPattern first second ->
            "AsPattern"
                ++ (first
                        |> Node.value
                        |> patternToString
                   )
                ++ " "
                ++ (second |> Node.value)

        ParenthesizedPattern inner ->
            "ParenPattern (" ++ (inner |> Node.value |> patternToString) ++ ")"


expressionToString expression =
    case expression of
        UnitExpr ->
            "Unit"

        Application expressions ->
            expressions
                |> List.map Node.value
                |> List.map expressionToString
                |> listToString "Application"

        OperatorApplication _ _ _ _ ->
            "OperatorApplication"

        FunctionOrValue moduleName name ->
            String.join "." (moduleName ++ [ name ])

        IfBlock _ _ _ ->
            "If"

        PrefixOperator _ ->
            "Prefix"

        Operator _ ->
            "Operator"

        Integer _ ->
            "Integer"

        Hex _ ->
            "Hex"

        Floatable _ ->
            "Floatable"

        Negation _ ->
            "Negation"

        Literal _ ->
            "Literal"

        CharLiteral _ ->
            "Charliteral"

        TupledExpression _ ->
            "Tupled"

        ParenthesizedExpression _ ->
            "ParenExpr"

        LetExpression _ ->
            "Let"

        CaseExpression _ ->
            "Case"

        LambdaExpression _ ->
            "Lambda"

        RecordExpr _ ->
            "RecordExpr"

        ListExpr _ ->
            "ListExpr"

        RecordAccess _ _ ->
            "RecordAccess"

        RecordAccessFunction _ ->
            "RecordAccess"

        RecordUpdateExpression _ _ ->
            "RecordUpdate"

        GLSLExpression _ ->
            "GLSL"
