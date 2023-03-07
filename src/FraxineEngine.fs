module FraxineEngine

open Browser.Types
open Browser.Dom
open Browser.Css
open Browser.Event

open Feliz
open Feliz.Styles

open Fable.Core
open FSharp.Data.Adaptive
open Fable.Core.JsInterop
open System.Text.RegularExpressions


type FraxineAttr =
    | Bool of bool
    | String of string


[<Struct>]
type FraxineCssprop = { key: string; value: string }

[<Struct>]
type FraxineStyleSheet =
    { selector: string
      properties: FraxineCssprop seq }

[<Struct>]
type FraxineInlineStyle = { properties: FraxineCssprop seq }

[<RequireQualifiedAccess>]
type FraxineNode =
    | Text of Text
    | Element of Element
    | Fragment of DocumentFragment
    | Attribute of string * FraxineAttr
    | Stylesheet of FraxineStyleSheet
    | InlineStyle of FraxineInlineStyle
    | Event of name: string * (Event -> unit)

let stringifyCssProp (prop: FraxineCssprop) = $"{prop.key}:{prop.value};"

let stringifyStyleSheet (stylesheet: FraxineStyleSheet) : string =
    let props =
        stylesheet.properties
        |> Seq.fold (fun current next -> $"{current}\n{stringifyCssProp next}") ""

    $"{stylesheet.selector} {{{props}}}"

let styleToConstructableStyleSheet styles =
    let stylesheet = CSSStyleSheet.Create()
    stringifyStyleSheet styles |> stylesheet.replaceSync
    stylesheet

let append (element: Node, node) = element.appendChild node |> ignore

let setAttribute (element: Element, attribute) = element.setAttribute attribute

let setupNode (parent: Element) child =
    let root: ShadowRoot option = parent?shadowRoot |> Option.ofObj

    let parent =
        root |> Option.map (fun sr -> sr :> Element) |> Option.defaultValue parent

    match child with
    | FraxineNode.Element child -> append (parent, child)
    | FraxineNode.Text text -> append (parent, text)
    | FraxineNode.Fragment fragment -> append (parent, fragment)
    | FraxineNode.Attribute(name, Bool true) -> setAttribute (parent, (name, ""))
    | FraxineNode.Attribute(name, Bool false) -> parent.removeAttribute (name)
    | FraxineNode.Attribute(name, String value) -> setAttribute (parent, (name, value))
    | FraxineNode.Event(name, handler) ->
        let name = name.ToLowerInvariant()
        parent.addEventListener (name, handler)

    | FraxineNode.Stylesheet stylesheet ->
        let style = styleToConstructableStyleSheet stylesheet

        let adopted = parent?adoptedStyleSheets |> Option.ofObj |> Option.defaultValue [||]
        parent?adoptedStyleSheets <- [| yield! adopted; style |]

    | FraxineNode.InlineStyle styles ->
        let styles =
            styles.properties
            |> Seq.fold (fun current next -> $"{current}{stringifyCssProp next}") ""

        parent.setAttribute ("style", styles)

let makeNode useShadowDom =
    fun tag (children: FraxineNode seq) ->
        let target = document.createElement tag

        match useShadowDom with
        | true -> target.attachShadow (unbox box {| mode = EncapsulationMode.Open |}) |> ignore
        | false -> ()

        children |> Seq.iter (setupNode target)
        target :> Element |> FraxineNode.Element

let makeText text =
    let el = document.createTextNode text
    el |> FraxineNode.Text

let makeFragment () =
    FraxineNode.Fragment(document.createDocumentFragment ())

let makeStrAttr name value =
    FraxineNode.Attribute(name, String value)

let makeBoolAttr name value = FraxineNode.Attribute(name, Bool value)
let makeEvent name handler = FraxineNode.Event(name, handler)
let makeCssProp key value = { key = key; value = value }

type FraxineEngine() =
    inherit HtmlEngine<FraxineNode>(makeNode false, makeText, makeFragment)

    member _.fragment = makeFragment ()

[<Literal>]
let validCustomElementName =
    @"^([a-z]+[-._0-9]+([.0-9]|\w)+|\barticle\b|\baside\b|\bblockquote\b|\bbody\b|\bdiv\b|\bfooter\b|(h{1}[1-6]{1})|\bheader\b|\bmain\b|\bnav\b|\bp\b|\bsection\b|\bspan\b)$"

type FraxineShadowEngine() =

    let ceNameregex = new Regex(validCustomElementName)

    let forbiddenNames =
        set
            [ "annotation-xml"
              "color-profile"
              "font-face"
              "font-face-src"
              "font-face-uri"
              "font-face-format"
              "font-face-name"
              "missing-glyph" ]

    member _.custom(tagname: string, children: FraxineNode seq) =
        if forbiddenNames |> Set.contains tagname then
            failwith "Invalid Element Name"

        if ceNameregex.IsMatch tagname then
            makeNode true tagname children
        else
            failwith "Invalid Element Name"

    member _.article(text) = makeText text
    member _.article(children) = makeNode true "article" children

    member _.aside(text) = makeText text
    member _.aside(children) = makeNode true "aside" children

    member _.blockquote(text) = makeText text
    member _.blockquote(children) = makeNode true "blockquote" children

    member _.div(text) = makeText text
    member _.div(children) = makeNode true "div" children

    member _.footer(text) = makeText text
    member _.footer(children) = makeNode true "footer" children

    member _.header(text) = makeText text
    member _.header(children) = makeNode true "header" children

    member _.h1(text) = makeText text
    member _.h1(children) = makeNode true "h1" children

    member _.h2(text) = makeText text
    member _.h2(children) = makeNode true "h2" children

    member _.h3(text) = makeText text
    member _.h3(children) = makeNode true "h3" children

    member _.h4(text) = makeText text
    member _.h4(children) = makeNode true "h4" children

    member _.h5(text) = makeText text
    member _.h5(children) = makeNode true "h5" children

    member _.h6(text) = makeText text
    member _.h6(children) = makeNode true "h6" children

    member _.main(text) = makeText text
    member _.main(children) = makeNode true "main" children

    member _.nav(children) = makeNode true "nav" children
    member _.nav(text) = makeText text

    member _.p(children) = makeNode true "p" children
    member _.p(text) = makeText text

    member _.section(text) = makeText text
    member _.section(children) = makeNode true "section" children

    member _.span(text) = makeText text
    member _.span(children) = makeNode true "span" children


    member _.stylesheet(selector: string, properties: FraxineCssprop seq) =
        { selector = selector
          properties = properties }
        |> FraxineNode.Stylesheet

type FraxineAttributeEngine() =
    inherit AttrEngine<FraxineNode>(makeStrAttr, makeBoolAttr)

    member _.style(props: FraxineCssprop seq) : FraxineNode =
        FraxineNode.InlineStyle { properties = props }

type FraxineEventEngine() =
    inherit EventEngine<FraxineNode>(makeEvent)

    member _.custom(name: string, handler: #Event -> unit) =
        let inline handler (event: Event) = handler (downcast event)

        FraxineNode.Event(name, handler)


/// Normal DOM elements
let Html = FraxineEngine()
/// DOM Elements with shadow roots attached to them
let Shadow = FraxineShadowEngine()
// DOM Events
let Event = EventEngine(makeEvent)
// DOM Events
let Attr = FraxineAttributeEngine()
//
let Css = CssEngine(makeCssProp)


type DOM =

    static member mount(target: Element, childContent) = setupNode target childContent

    static member mount(target: string, node) =
        let target = document.querySelector target |> Option.ofObj

        match target with
        | None -> eprintfn "Target not found"
        | Some target -> DOM.mount (target, node)
