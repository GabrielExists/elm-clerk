module Kernel.Html exposing (Attr(..), Html(..), attribute, htmlToReal, node, nodeNS, style, text)

import Html
import Html.Attributes
import InterpreterTypes exposing (Value(..))
import Json.Encode
import Value
import VirtualDom


type Html
    = HtmlPlain String (List Attr) (List Html)
    | HtmlNS String String (List Attr) (List Html)
    | Text String


type Attr
    = Style String String
    | Attribute String String
    | Property String Value


htmlToReal : Html -> Html.Html msg
htmlToReal html =
    case html of
        HtmlPlain name attrs children ->
            Html.node name
                (attrs |> List.map attrToReal)
                (children |> List.map htmlToReal)

        HtmlNS namespace tag attrs children ->
            VirtualDom.nodeNS namespace
                tag
                (attrs |> List.map attrToReal)
                (children |> List.map htmlToReal)

        Text string ->
            Html.text string


attrToReal : Attr -> Html.Attribute msg
attrToReal attr =
    case attr of
        Style first second ->
            Html.Attributes.style first second

        Attribute first second ->
            Html.Attributes.attribute first second

        Property key (String value) ->
            Html.Attributes.property key (Json.Encode.string value)

        Property first second ->
            second
                |> Value.toString
                |> Json.Encode.string
                |> Html.Attributes.property first


node : String -> List Attr -> List Html -> Html
node name attrs nodes =
    HtmlPlain name attrs nodes


nodeNS : String -> String -> List Attr -> List Html -> Html
nodeNS namespace tag attrs nodes =
    HtmlNS namespace tag attrs nodes


text : String -> Html
text string =
    Text string



--map : (a -> msg) -> Html -> Html
--map _ inputNode =
--    inputNode


style : String -> String -> Attr
style first second =
    Style first second


property : String -> Value -> Attr
property key value =
    Property key value


attribute : String -> String -> Attr
attribute key value =
    Attribute key value



--attribute : String -> String -> Attribute msg
--attribute key value =
--    Elm.Kernel.VirtualDom.attribute
--        (Elm.Kernel.VirtualDom.noOnOrFormAction key)
--        (Elm.Kernel.VirtualDom.noJavaScriptOrHtmlUri value)
--
--
--attributeNS : String -> String -> String -> Attribute msg
--attributeNS namespace key value =
--    Elm.Kernel.VirtualDom.attributeNS
--        namespace
--        (Elm.Kernel.VirtualDom.noOnOrFormAction key)
--        (Elm.Kernel.VirtualDom.noJavaScriptOrHtmlUri value)
--
--
--mapAttribute : (a -> b) -> Attribute a -> Attribute b
--mapAttribute =
--    Elm.Kernel.VirtualDom.mapAttribute
--
--
--on : String -> Handler msg -> Attribute msg
--on =
--    Elm.Kernel.VirtualDom.on
--
--
--type Handler msg
--    = Normal (Json.Decoder msg)
--    | MayStopPropagation (Json.Decoder ( msg, Bool ))
--    | MayPreventDefault (Json.Decoder ( msg, Bool ))
--    | Custom (Json.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool })
--
--
--lazy : (a -> Node msg) -> a -> Node msg
--lazy =
--    Elm.Kernel.VirtualDom.lazy
--
--
--lazy2 : (a -> b -> Node msg) -> a -> b -> Node msg
--lazy2 =
--    Elm.Kernel.VirtualDom.lazy2
--
--
--lazy3 : (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
--lazy3 =
--    Elm.Kernel.VirtualDom.lazy3
--
--
--lazy4 : (a -> b -> c -> d -> Node msg) -> a -> b -> c -> d -> Node msg
--lazy4 =
--    Elm.Kernel.VirtualDom.lazy4
--
--
--lazy5 : (a -> b -> c -> d -> e -> Node msg) -> a -> b -> c -> d -> e -> Node msg
--lazy5 =
--    Elm.Kernel.VirtualDom.lazy5
--
--
--lazy6 : (a -> b -> c -> d -> e -> f -> Node msg) -> a -> b -> c -> d -> e -> f -> Node msg
--lazy6 =
--    Elm.Kernel.VirtualDom.lazy6
--
--
--lazy7 : (a -> b -> c -> d -> e -> f -> g -> Node msg) -> a -> b -> c -> d -> e -> f -> g -> Node msg
--lazy7 =
--    Elm.Kernel.VirtualDom.lazy7
--
--
--lazy8 : (a -> b -> c -> d -> e -> f -> g -> h -> Node msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Node msg
--lazy8 =
--    Elm.Kernel.VirtualDom.lazy8
--
--
--keyedNode : String -> List (Attribute msg) -> List ( String, Node msg ) -> Node msg
--keyedNode tag =
--    Elm.Kernel.VirtualDom.keyedNode (Elm.Kernel.VirtualDom.noScript tag)
--
--
--keyedNodeNS : String -> String -> List (Attribute msg) -> List ( String, Node msg ) -> Node msg
--keyedNodeNS namespace tag =
--    Elm.Kernel.VirtualDom.keyedNodeNS namespace (Elm.Kernel.VirtualDom.noScript tag)
