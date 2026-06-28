module Kernel exposing (EvalFunction, InSelector, OutSelector, Selector, To, anything, char, encodedValue, four, function, functionList, html, int, list, listIn, maybe, one, string, three, to, tuple, tupleIn, two)

import Array exposing (Array)
import Bitwise
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import Environment
import EvalResult
import FastDict as Dict exposing (Dict)
import InterpreterTypes exposing (Eval, EvalErrorData, EvalResult, PartiallyAppliedFunction(..), Value(..))
import Kernel.Debug
import Kernel.Html exposing (Attr(..), Html(..))
import Kernel.JsArray
import Kernel.String
import Kernel.Utils
import Maybe.Extra
import Value exposing (typeError)


type alias EvalFunction =
    List Value
    -> List (Node Pattern)
    -> Maybe QualifiedNameRef
    -> Node Expression
    -> Eval Value


functionList : EvalFunction -> Dict ModuleName (Dict String ( Int, List Value -> Eval Value ))
functionList evalFunction =
    [ -- Elm.Kernel.Basics
      ( [ "Elm", "Kernel", "Basics" ]
      , [ ( "acos", one float to float acos )
        , ( "add", twoNumbers (+) (+) )
        , ( "and", two bool bool to bool (&&) )
        , ( "asin", one float to float asin )
        , ( "atan", one float to float atan )
        , ( "atan2", two float float to float atan2 )
        , ( "ceiling", one float to int ceiling )
        , ( "cos", one float to float cos )
        , ( "e", constant float e )
        , ( "fdiv", two float float to float (/) )
        , ( "floor", one float to int floor )
        , ( "idiv", two int int to int (//) )
        , ( "isInfinite", one float to bool isInfinite )
        , ( "isNaN", one float to bool isNaN )
        , ( "log", one float to float (logBase e) )
        , ( "modBy", two int int to int modBy )
        , ( "mul", twoNumbers (*) (*) )
        , ( "not", one bool to bool not )
        , ( "or", two bool bool to bool (||) )
        , ( "pi", constant float pi )
        , ( "pow", twoNumbers (^) (^) )
        , ( "remainderBy", two int int to int remainderBy )
        , ( "round", one float to int round )
        , ( "sin", one float to float sin )
        , ( "sqrt", one float to float sqrt )
        , ( "sub", twoNumbers (-) (-) )
        , ( "tan", one float to float tan )
        , ( "toFloat", one int to float toFloat )
        , ( "truncate", one float to int truncate )
        , ( "xor", two bool bool to bool xor )
        ]
      )

    -- Elm.Kernel.Bitwise
    , ( [ "Elm", "Kernel", "Bitwise" ]
      , [ ( "and", two int int to int Bitwise.and )
        , ( "complement", one int to int Bitwise.complement )
        , ( "or", two int int to int Bitwise.or )
        , ( "shiftLeftBy", two int int to int Bitwise.shiftLeftBy )
        , ( "shiftRightBy", two int int to int Bitwise.shiftRightBy )
        , ( "shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy )
        , ( "xor", two int int to int Bitwise.xor )
        ]
      )

    -- Elm.Kernel.Char
    , ( [ "Elm", "Kernel", "Char" ]
      , [ ( "fromCode", one int to char Char.fromCode )
        , ( "toCode", one char to int Char.toCode )
        , ( "toLocaleLower", one char to char Char.toLocaleLower )
        , ( "toLocaleUpper", one char to char Char.toLocaleUpper )
        , ( "toLower", one char to char Char.toLower )
        , ( "toUpper", one char to char Char.toUpper )
        ]
      )

    -- Elm.Kernel.Debug
    , ( [ "Elm", "Kernel", "Debug" ]
      , [ ( "log", twoWithError string anything to anything Kernel.Debug.log )
        , ( "toString", one anything to string Value.toString )
        , ( "todo", oneWithError string to anything Kernel.Debug.todo )
        ]
      )

    -- Elm.Kernel.JsArray
    , ( [ "Elm", "Kernel", "JsArray" ]
      , [ ( "appendN", three int (jsArray anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.appendN )
        , ( "empty", zero to (jsArray anything) Array.empty )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldr )
        , ( "foldl", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldl )
        , ( "initialize", threeWithError int int (function evalFunction int to anything) to (jsArray anything) Kernel.JsArray.initialize )
        , ( "initializeFromList", two int anyList to (tuple (jsArray anything) anyList) Kernel.JsArray.initializeFromList )
        , ( "length", one (jsArray anything) to int Array.length )
        , ( "map", twoWithError (function evalFunction anything to anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.map )
        , ( "indexedMap", twoWithError (function2 evalFunction int anything to anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.indexedMap )
        , ( "push", two anything (jsArray anything) to (jsArray anything) Array.push )
        , ( "slice", three int int (jsArray anything) to (jsArray anything) Array.slice )
        , ( "singleton", one anything to (jsArray anything) (List.singleton >> Array.fromList) )
        , ( "unsafeGet", twoWithError int (jsArray anything) to anything Kernel.JsArray.unsafeGet )
        , ( "unsafeSet", three int anything (jsArray anything) to (jsArray anything) Array.set )
        ]
      )

    -- Elm.Kernel.List
    , ( [ "Elm", "Kernel", "List" ]
      , [ ( "cons", two anything anyList to anyList (::) )
        , ( "fromArray", one (jsArray anything) to anyList Array.toList )
        , ( "toArray", one anyList to (jsArray anything) Array.fromList )
        ]
      )

    -- Elm.Kernel.String
    , ( [ "Elm", "Kernel", "String" ]
      , [ ( "length", one string to int String.length )
        , ( "toFloat", one string to (maybe float) String.toFloat )
        , ( "toInt", one string to (maybe int) String.toInt )
        , ( "toLower", one string to string String.toLower )
        , ( "toUpper", one string to string String.toUpper )
        , ( "append", two string string to string String.append )
        , ( "cons", two char string to string String.cons )
        , ( "contains", two string string to bool String.contains )
        , ( "endsWith", two string string to bool String.endsWith )
        , ( "filter", twoWithError (function evalFunction char to bool) string to string Kernel.String.filter )
        , ( "foldl", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldl )
        , ( "foldr", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldr )
        , ( "fromList", one (list char) to string String.fromList )
        , ( "fromNumber", oneWithError anything to string Kernel.String.fromNumber ) -- TODO: `fromFloat` is not the same as `fromNumber`
        , ( "indexes", two string string to (list int) String.indexes )
        , ( "join", two string (jsArray string) to string (\s a -> String.join s (Array.toList a)) )
        , ( "lines", one string to (list string) String.lines )
        , ( "reverse", one string to string String.reverse )
        , ( "slice", three int int string to string String.slice )
        , ( "split", two string string to (jsArray string) (\s l -> Array.fromList (String.split s l)) )
        , ( "startsWith", two string string to bool String.startsWith )
        , ( "trim", one string to string String.trim )
        , ( "trimLeft", one string to string String.trimLeft )
        , ( "trimRight", one string to string String.trimRight )
        , ( "uncons", one string to (maybe (tuple char string)) String.uncons )
        , ( "words", one string to (list string) String.words )
        ]
      )

    -- Elm.Kernel.Utils
    , ( [ "Elm", "Kernel", "Utils" ]
      , [ ( "append", twoWithError anything anything to anything Kernel.Utils.append )
        , ( "ge", Kernel.Utils.comparison [ GT, EQ ] )
        , ( "gt", Kernel.Utils.comparison [ GT ] )
        , ( "le", Kernel.Utils.comparison [ LT, EQ ] )
        , ( "lt", Kernel.Utils.comparison [ LT ] )
        , ( "equal", Kernel.Utils.comparison [ EQ ] )
        , ( "notEqual", Kernel.Utils.comparison [ LT, GT ] )
        , ( "compare", twoWithError anything anything to order Kernel.Utils.compare )
        ]
      )

    --style : String -> String -> Attribute msg
    --style =
    --  Elm.Kernel.VirtualDom.style
    -- Elm.Kernel.VirtualDom
    , ( [ "Elm", "Kernel", "VirtualDom" ]
      , [ ( "node", three string (list attr) (list html) to html Kernel.Html.node )
        , ( "nodeNS", four string string (list attr) (list html) to html Kernel.Html.nodeNS )
        , ( "text", one string to html Kernel.Html.text )
        , ( "style", two string string to attr Kernel.Html.style )
        , ( "attribute", two string string to attr Kernel.Html.attribute )
        ]
      )
    ]
        |> List.map
            (\( moduleName, moduleFunctions ) ->
                ( moduleName
                , moduleFunctions
                    |> List.map (\( k, f ) -> ( k, f moduleName k ))
                    |> Dict.fromList
                )
            )
        |> Dict.fromList



-- Selectors


type alias InSelector a x =
    { x
        | fromValue : Value -> Maybe a
        , name : String
    }


type alias OutSelector a x =
    { x
        | toValue : a -> Value
        , name : String
    }


type alias Selector a =
    InSelector a (OutSelector a {})


type To
    = To


to : To
to =
    To


anything : Selector Value
anything =
    { fromValue = Just
    , toValue = identity
    , name = "anything"
    }


order : Selector Order
order =
    { fromValue = Value.toOrder
    , toValue = Value.fromOrder
    , name = "Order"
    }


string : Selector String
string =
    { fromValue =
        \value ->
            case value of
                String s ->
                    Just s

                _ ->
                    Nothing
    , toValue = String
    , name = "String"
    }


float : Selector Float
float =
    { fromValue =
        \value ->
            case value of
                Float s ->
                    Just s

                Int i ->
                    -- Stuff like "2 / 3" is parsed as (Int 2) / (Int 3)
                    Just (toFloat i)

                _ ->
                    Nothing
    , toValue = Float
    , name = "Float"
    }


int : Selector Int
int =
    { fromValue =
        \value ->
            case value of
                Int s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Int
    , name = "Int"
    }


char : Selector Char
char =
    { fromValue =
        \value ->
            case value of
                Char s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Char
    , name = "Char"
    }


bool : Selector Bool
bool =
    { fromValue =
        \value ->
            case value of
                Bool s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Bool
    , name = "Bool"
    }


maybe : Selector a -> Selector (Maybe a)
maybe selector =
    { fromValue =
        \value ->
            case value of
                Custom [ "Maybe" ] "Nothing" [] ->
                    Just Nothing

                Custom [ "Maybe" ] "Just" [ arg ] ->
                    Maybe.map Just (selector.fromValue arg)

                _ ->
                    Nothing
    , toValue =
        \maybeValue ->
            case maybeValue of
                Nothing ->
                    Value.nothingValue

                Just value ->
                    Custom [ "Maybe" ] "Just" [ selector.toValue value ]
    , name = "Maybe " ++ selector.name
    }


listIn : InSelector a inner -> InSelector (List a) {}
listIn selector =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Maybe.Extra.combineMap selector.fromValue l

                _ ->
                    Nothing
    , name = "List " ++ selector.name
    }


listOut : OutSelector a inner -> OutSelector (List a) {}
listOut selector =
    { toValue =
        \value ->
            value
                |> List.map selector.toValue
                |> List
    , name = "List " ++ selector.name
    }


list : Selector a -> Selector (List a)
list selector =
    combinedInOut (listIn selector) (listOut selector)


anyList : Selector (List Value)
anyList =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Just l

                _ ->
                    Nothing
    , toValue = List
    , name = "List anything"
    }


jsArray : Selector a -> Selector (Array a)
jsArray selector =
    { fromValue =
        \value ->
            case value of
                JsArray jsa ->
                    jsa
                        |> Array.toList
                        |> Maybe.Extra.combineMap selector.fromValue
                        |> Maybe.map Array.fromList

                _ ->
                    Nothing
    , toValue =
        \array ->
            array
                |> Array.map selector.toValue
                |> JsArray
    , name = "JsArray " ++ selector.name
    }


encodedValue : Selector Value
encodedValue =
    let
        fromValue : Value -> Maybe Value
        fromValue value =
            case value of
                Custom [] "String" [ item ] ->
                    string.fromValue item |> Maybe.map String

                Custom [] "Int" [ item ] ->
                    int.fromValue item |> Maybe.map Int

                Custom [] "Float" [ item ] ->
                    float.fromValue item |> Maybe.map Float

                Custom [] "Char" [ item ] ->
                    char.fromValue item |> Maybe.map Char

                Custom [] "Bool" [ item ] ->
                    bool.fromValue item |> Maybe.map Bool

                Custom [] "Unit" [] ->
                    Just Unit

                Custom [] "Tuple" [ first, second ] ->
                    Maybe.map2 Tuple (encodedValue.fromValue first) (encodedValue.fromValue second)

                Custom [] "Triple" [ first, second, third ] ->
                    Maybe.map3 Triple (encodedValue.fromValue first) (encodedValue.fromValue second) (encodedValue.fromValue third)

                Custom [] "Record" [] ->
                    Nothing

                Custom [] "Custom" [ moduleName, name, values ] ->
                    Maybe.map3 Custom
                        ((list string).fromValue moduleName)
                        (string.fromValue name)
                        ((list encodedValue).fromValue values)

                Custom [] "PartiallyApplied" [] ->
                    Nothing

                Custom [] "JsArray" [] ->
                    Nothing

                Custom [] "List" [ values ] ->
                    Maybe.map List <|
                        (list encodedValue).fromValue values

                _ ->
                    Nothing

        toValue : Value -> Value
        toValue value =
            case value of
                String item ->
                    Custom [] "String" [ string.toValue item ]

                Int item ->
                    Custom [] "Int" [ int.toValue item ]

                Float item ->
                    Custom [] "Float" [ float.toValue item ]

                Char item ->
                    Custom [] "Char" [ char.toValue item ]

                Bool item ->
                    Custom [] "Bool" [ bool.toValue item ]

                Unit ->
                    Custom [] "Unit" []

                Tuple first second ->
                    Custom [] "Tuple" [ encodedValue.toValue first, encodedValue.toValue second ]

                Triple first second third ->
                    Custom [] "Triple" [ encodedValue.toValue first, encodedValue.toValue second, encodedValue.toValue third ]

                Record _ ->
                    Custom [] "Record" []

                Custom moduleName name values ->
                    Custom [] "Custom" [ (list string).toValue moduleName, string.toValue name, (list encodedValue).toValue values ]

                PartiallyApplied (PartiallyAppliedFunction _ _ _ _ _) ->
                    Custom [] "PartiallyApplied" []

                JsArray _ ->
                    Custom [] "JsArray" []

                List values ->
                    Custom [] "List" [ (list encodedValue).toValue values ]
    in
    { fromValue = fromValue
    , toValue = toValue
    , name = "Encoded Value"
    }


function :
    EvalFunction
    -> OutSelector from xf
    -> To
    -> InSelector to xt
    -> InSelector (from -> Eval to) {}
function evalFunctionWith inSelector _ outSelector =
    let
        fromValue : Value -> Maybe (from -> Eval to)
        fromValue value =
            case value of
                PartiallyApplied (PartiallyAppliedFunction localEnv oldArgs patterns maybeName implementation) ->
                    Just
                        (\arg cfg _ ->
                            evalFunctionWith (oldArgs ++ [ inSelector.toValue arg ]) patterns maybeName implementation cfg localEnv
                                |> EvalResult.onValue
                                    (\out ->
                                        case outSelector.fromValue out of
                                            Just ov ->
                                                Ok ov

                                            Nothing ->
                                                Err <|
                                                    typeError localEnv <|
                                                        "Could not convert output from "
                                                            ++ Value.toString out
                                                            ++ " to "
                                                            ++ outSelector.name
                                    )
                        )

                _ ->
                    Nothing
    in
    { name = inSelector.name ++ " -> " ++ outSelector.name
    , fromValue = fromValue
    }


function2 :
    EvalFunction
    -> OutSelector a xa
    -> OutSelector b xb
    -> To
    -> InSelector to xt
    -> InSelector (a -> Eval (b -> Eval to)) {}
function2 evalFunction in1Selector in2Selector _ outSelector =
    function evalFunction in1Selector to (function evalFunction in2Selector to outSelector)



--tupleIn : { a | fromValue : Value -> Maybe b, name : String } -> { c | fromValue : Value -> Maybe d, name : String } -> { fromValue : Value -> Maybe (b, d), name : String }


tupleIn : InSelector a inner1 -> InSelector b inner2 -> InSelector ( a, b ) {}
tupleIn firstSelector secondSelector =
    { fromValue =
        \value ->
            case value of
                Tuple first second ->
                    Maybe.map2 Tuple.pair (firstSelector.fromValue first) (secondSelector.fromValue second)

                _ ->
                    Nothing
    , name = "( " ++ firstSelector.name ++ ", " ++ secondSelector.name ++ ")"
    }


tupleOut : OutSelector a inner1 -> OutSelector b inner2 -> OutSelector ( a, b ) {}
tupleOut firstSelector secondSelector =
    { toValue =
        \( first, second ) ->
            Tuple (firstSelector.toValue first) (secondSelector.toValue second)
    , name = "( " ++ firstSelector.name ++ ", " ++ secondSelector.name ++ ")"
    }


tuple : Selector a -> Selector b -> Selector ( a, b )
tuple firstSelector secondSelector =
    combinedInOut (tupleIn firstSelector secondSelector) (tupleOut firstSelector secondSelector)


constant : OutSelector res x -> res -> ModuleName -> String -> ( Int, List Value -> Eval Value )
constant selector const _ _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.succeed <| selector.toValue const

            _ ->
                EvalResult.fail <| typeError env <| "Didn't expect any args"
    )


html : Selector Html
html =
    { fromValue =
        \value ->
            case value of
                Custom [ "Html" ] "Node" [ String name, attrsValue, nodesValue ] ->
                    Maybe.map2 (HtmlPlain name)
                        (attrsValue |> (list attr).fromValue)
                        (nodesValue |> (list html).fromValue)

                Custom [ "Html" ] "NodeNS" [ String namespace, String tag, attrsValue, nodesValue ] ->
                    Maybe.map2 (HtmlNS namespace tag)
                        (attrsValue |> (list attr).fromValue)
                        (nodesValue |> (list html).fromValue)

                Custom [ "Html" ] "Text" [ String text ] ->
                    Text text |> Just

                _ ->
                    Nothing
    , toValue =
        \node ->
            case node of
                HtmlPlain name attrs htmls ->
                    Custom [ "Html" ]
                        "Node"
                        [ name |> string.toValue
                        , attrs |> (list attr).toValue
                        , htmls |> (list html).toValue
                        ]

                HtmlNS namespace tag attrs htmls ->
                    Custom [ "Html" ]
                        "NodeNS"
                        [ namespace |> string.toValue
                        , tag |> string.toValue
                        , attrs |> (list attr).toValue
                        , htmls |> (list html).toValue
                        ]

                Text text ->
                    Custom [ "Html" ] "Text" [ String text ]
    , name = "Html.Node"
    }


attr : Selector Attr
attr =
    { fromValue =
        \value ->
            case value of
                Custom [ "Html" ] "Style" [ String first, String second ] ->
                    Style first second |> Just

                Custom [ "Html" ] "Attribute" [ String first, String second ] ->
                    Attribute first second |> Just

                Custom [ "Html" ] "Property" [ String first, secondValue ] ->
                    Property first secondValue |> Just

                _ ->
                    Nothing
    , toValue =
        \item ->
            case item of
                Style first second ->
                    Custom [ "Html" ] "Style" [ String first, String second ]

                Attribute first second ->
                    Custom [ "Html" ] "Attribute" [ String first, String second ]

                Property first second ->
                    Custom [ "Html" ] "Property" [ String first, second ]
    , name = "Html.Attr"
    }


zero :
    To
    -> OutSelector out ox
    -> out
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
zero _ output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> OutSelector out ox
    -> Result EvalErrorData out
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
zeroWithError _ output f _ _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.fromResult <| Result.map output.toValue f

            _ ->
                EvalResult.fail <| typeError env <| "Expected zero args, got more"
    )


one :
    InSelector a ax
    -> To
    -> OutSelector out ox
    -> (a -> out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
one firstSelector _ output f =
    oneWithError firstSelector To output (\v _ _ -> EvalResult.succeed (f v))


oneWithError :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> Eval out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
oneWithError firstSelector _ output f moduleName functionName =
    ( 1
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", got " ++ got
        in
        case args of
            [ arg ] ->
                case firstSelector.fromValue arg of
                    Just s ->
                        f s cfg env
                            |> EvalResult.map output.toValue

                    Nothing ->
                        err (Value.toString arg)

            [] ->
                partiallyApply moduleName functionName args

            _ ->
                err "more"
    )


two :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
two firstSelector secondSelector _ output f =
    twoWithError firstSelector secondSelector To output (\l r _ _ -> EvalResult.succeed (f l r))


twoWithError :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> Eval out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
twoWithError firstSelector secondSelector _ output f moduleName functionName =
    ( 2
    , \args cfg env ->
        let
            typeError_ : String -> EvalResult value
            typeError_ msg =
                EvalResult.fail (typeError env msg)
        in
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        typeError_ <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                typeError_ <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                f first second cfg env
                                    |> EvalResult.map output.toValue

            [ _ ] ->
                partiallyApply moduleName functionName args

            [] ->
                partiallyApply moduleName functionName args

            _ ->
                let
                    got : String
                    got =
                        String.join ", " <| List.map Value.toString args
                in
                if firstSelector.name == secondSelector.name then
                    typeError_ <| "Expected two " ++ firstSelector.name ++ "s, got " ++ got

                else
                    typeError_ <| "Expected one " ++ firstSelector.name ++ " and one " ++ secondSelector.name ++ ", got " ++ got
    )


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
three firstSelector secondSelector thirdSelector _ output f =
    threeWithError firstSelector secondSelector thirdSelector To output (\l m r _ _ -> EvalResult.succeed (f l m r))


threeWithError :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> Eval out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
threeWithError firstSelector secondSelector thirdSelector _ output f moduleName functionName =
    ( 3
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name && secondSelector.name == thirdSelector.name then
                    EvalResult.fail <| typeError env <| "Expected three " ++ firstSelector.name ++ "s, got " ++ got

                else
                    EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", one " ++ secondSelector.name ++ " and one " ++ thirdSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector.fromValue firstArg, secondSelector.fromValue secondArg, thirdSelector.fromValue thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        f first second third cfg env
                            |> EvalResult.map output.toValue

                    _ ->
                        err (String.join ", " (List.map Value.toString args))

            [ _, _ ] ->
                partiallyApply moduleName functionName args

            [ _ ] ->
                partiallyApply moduleName functionName args

            [] ->
                partiallyApply moduleName functionName args

            _ ->
                err ("[ " ++ String.join ", " (List.map Value.toString args) ++ " ]")
    )


four :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> InSelector d xd
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> d -> out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
four firstSelector secondSelector thirdSelector fourthSelector _ output f =
    fourWithError firstSelector secondSelector thirdSelector fourthSelector To output (\l m1 m2 r _ _ -> EvalResult.succeed (f l m1 m2 r))


fourWithError :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> InSelector d xd
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> d -> Eval out)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
fourWithError firstSelector secondSelector thirdSelector fourthSelector _ output f moduleName functionName =
    ( 3
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name && secondSelector.name == thirdSelector.name && thirdSelector.name == fourthSelector.name then
                    EvalResult.fail <| typeError env <| "Expected four " ++ firstSelector.name ++ "s, got " ++ got

                else
                    EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", one " ++ secondSelector.name ++ ", one " ++ thirdSelector.name ++ "  and one " ++ fourthSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg, thirdArg, fourthArg ] ->
                case ( ( firstSelector.fromValue firstArg, secondSelector.fromValue secondArg ), ( thirdSelector.fromValue thirdArg, fourthSelector.fromValue fourthArg ) ) of
                    ( ( Just first, Just second ), ( Just third, Just fourth ) ) ->
                        f first second third fourth cfg env
                            |> EvalResult.map output.toValue

                    _ ->
                        err (String.join ", " (List.map Value.toString args))

            [ _, _, _ ] ->
                partiallyApply moduleName functionName args

            [ _, _ ] ->
                partiallyApply moduleName functionName args

            [ _ ] ->
                partiallyApply moduleName functionName args

            [] ->
                partiallyApply moduleName functionName args

            _ ->
                err ("[ " ++ String.join ", " (List.map Value.toString args) ++ " ]")
    )


partiallyApply : ModuleName -> String -> List Value -> EvalResult Value
partiallyApply moduleName functionName args =
    EvalResult.fromResult <|
        Ok <|
            PartiallyApplied
                (PartiallyAppliedFunction
                    (Environment.empty moduleName)
                    args
                    []
                    (Just
                        { moduleName = moduleName
                        , name = functionName
                        }
                    )
                    (Node.empty (Expression.FunctionOrValue moduleName functionName))
                )


twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> ModuleName
    -> String
    -> ( Int, List Value -> Eval Value )
twoNumbers fInt fFloat moduleName functionName =
    ( 2
    , \args _ env ->
        case args of
            [ Int li, Int ri ] ->
                EvalResult.succeed <| Int (fInt li ri)

            [ Int li, Float rf ] ->
                EvalResult.succeed <| Float (fFloat (toFloat li) rf)

            [ Float lf, Int ri ] ->
                EvalResult.succeed <| Float (fFloat lf (toFloat ri))

            [ Float lf, Float rf ] ->
                EvalResult.succeed <| Float (fFloat lf rf)

            [ _ ] ->
                partiallyApply moduleName functionName args

            [] ->
                partiallyApply moduleName functionName args

            _ ->
                EvalResult.fail <| typeError env "Expected two numbers"
    )


combinedInOut : InSelector a inner1 -> OutSelector a inner2 -> Selector a
combinedInOut inSelector outSelector =
    { name = inSelector.name
    , fromValue = inSelector.fromValue
    , toValue = outSelector.toValue
    }
