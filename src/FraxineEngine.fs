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
open System

[<Emit("typeof $0 === $1")>]
let jsTypeof obj (type': string) : bool = jsNative

type ProxyGetTrap<'Target> =

  abstract get: target: 'Target * property: string * receiver: obj -> obj


type Proxy =

  [<Emit("new Proxy($0, $1)")>]
  static member Create<'Target, 'Handler>
    (
      target: 'Target,
      handler: 'Handler
    ) : 'Target =
    jsNative

type Reflect =

  [<Emit("Reflect.get($0, $1)")>]
  static member get<'Target, 'TOutput>
    (
      target: 'Target,
      property: string
    ) : 'TOutput =
    jsNative


type FraxineHtmlElement =
  inherit HTMLElement

  abstract __subscriptions: (IDisposable seq) option


let getProxiedElement (el: Element) =
  let trap =
    { new ProxyGetTrap<FraxineHtmlElement> with
        member _.get
          (
            target: FraxineHtmlElement,
            property: string,
            receiver: obj
          ) : obj =
          let invokedProp = target?(property)

          if jsTypeof invokedProp "function" then
            target?(property) <- invokedProp?bind (target)

          match property with
          | "remove" ->
            let target = target

            match target.__subscriptions with
            | Some subs ->
              for sub in subs do
                sub.Dispose()
            | None -> ()

            Reflect.get (target, property)
          | prop -> Reflect.get (target, prop)
    }

  Proxy.Create(el, trap)



type FraxineAttr =
  | Bool of boolvalue: bool
  | String of stringvalue: string
  | AdaptiveBool of boolValue: aval<bool>
  | AdaptiveString of boolValue: aval<string>

type FraxineCssprop = { key: string; value: string }


type FraxineStyleSheet = {
  selector: string
  properties: FraxineCssprop seq
}


type FraxineInlineStyle = { properties: FraxineCssprop seq }

[<RequireQualifiedAccess>]
type FraxineNode =
  | Text of string
  | Element of Element
  | Fragment of DocumentFragment
  | Attribute of attrName: string * attrValue: FraxineAttr
  | Stylesheet of FraxineStyleSheet
  | InlineStyle of FraxineInlineStyle
  | Event of name: string * handler: (Event -> unit)
  | AdaptiveText of aval<string>
  | AdaptiveElement of aval<Element>
  | AdaptiveFragment of aval<DocumentFragment>
  | AdaptiveStylesheet of aval<FraxineStyleSheet>
  | AdaptiveInlineStyle of aval<FraxineInlineStyle>

let stringifyCssProp (prop: FraxineCssprop) = $"{prop.key}:{prop.value};"

let stringifyStyleSheet (stylesheet: FraxineStyleSheet) : string =
  let props =
    stylesheet.properties
    |> Seq.fold (fun current next -> $"{current}\n{stringifyCssProp next}") ""

  $"{stylesheet.selector} {{{props}}}"

let cssFromFss styles =
  let stylesheet = CSSStyleSheet.Create()
  stringifyStyleSheet styles |> stylesheet.replaceSync
  stylesheet

let append (element: Node, node) = element.appendChild node |> ignore

let replace (parent: Node, newEl, oldEl) =
  parent.replaceChild (newEl, oldEl) |> ignore

let setAttribute (element: Element, attribute) = element.setAttribute attribute

type Cell<'TValue>(?initial: 'TValue) =
  let mutable _value = initial

  member _.Value
    with get () = _value
    and set (value: 'TValue option) = _value <- value


let setupNode (parent: Element) child =
  let shadowRoot = parent?shadowRoot |> Option.ofObj
  let parent = getProxiedElement parent

  let shadowOrParent = shadowRoot |> Option.defaultValue parent

  let subs = ResizeArray<IDisposable>()

  parent?__subscriptions <- subs

  match child with
  | FraxineNode.Element child -> append (shadowOrParent, child)
  | FraxineNode.Text text ->
    append (shadowOrParent, document.createTextNode (text))
  | FraxineNode.Fragment fragment -> append (shadowOrParent, fragment)
  | FraxineNode.Attribute(name, Bool true) ->
    setAttribute (shadowOrParent, (name, ""))
  | FraxineNode.Attribute(name, Bool false) ->
    shadowOrParent.removeAttribute (name)
  | FraxineNode.Attribute(name, AdaptiveBool value) ->
    let initial = AVal.force value

    if initial then
      setAttribute (shadowOrParent, (name, ""))

    value.AddCallback(fun value ->
      if value then
        setAttribute (shadowOrParent, (name, ""))
      else
        shadowOrParent.removeAttribute (name))
    |> subs.Add

  | FraxineNode.Attribute(name, AdaptiveString value) ->
    let initial = AVal.force value

    if not (String.IsNullOrWhiteSpace initial) then
      setAttribute (shadowOrParent, (name, initial))

    value.AddCallback(fun value ->
      if not (String.IsNullOrWhiteSpace initial) then
        setAttribute (shadowOrParent, (name, value))
      else
        shadowOrParent.removeAttribute (name))
    |> subs.Add

  | FraxineNode.Attribute(name, String value) ->
    setAttribute (shadowOrParent, (name, value))
  | FraxineNode.Event(name, handler) ->
    let name = name.ToLowerInvariant()
    shadowOrParent.addEventListener (name, handler)

  | FraxineNode.Stylesheet stylesheet ->
    let style = cssFromFss stylesheet

    let adopted =
      shadowOrParent?adoptedStyleSheets
      |> Option.ofObj
      |> Option.defaultValue [||]

    match shadowRoot with
    | Some root -> root?adoptedStyleSheets <- [| yield! adopted; style |]
    | None -> document?adoptedStyleSheets <- [| yield! adopted; style |]

    ()
  | FraxineNode.InlineStyle styles ->
    let styles =
      styles.properties
      |> Seq.fold (fun current next -> $"{current}{stringifyCssProp next}") ""

    shadowOrParent.setAttribute ("style", styles)

  | FraxineNode.AdaptiveText value ->
    let content = AVal.force value
    let textNode = document.createTextNode (content)
    append (shadowOrParent, textNode)

    value.AddCallback(fun (value: string) -> textNode.textContent <- value)
    |> subs.Add
  | FraxineNode.AdaptiveElement value -> ()
  | FraxineNode.AdaptiveFragment value -> ()
  | FraxineNode.AdaptiveStylesheet value -> ()
  | FraxineNode.AdaptiveInlineStyle value -> ()




let makeNode useShadowDom =
  fun tag (children: FraxineNode seq) ->
    let target = document.createElement tag

    match useShadowDom with
    | true ->
      target.attachShadow (unbox box {| mode = EncapsulationMode.Open |})
      |> ignore
    | false -> ()

    children |> Seq.iter (setupNode target)
    target :> Element |> FraxineNode.Element

let makeText text = FraxineNode.Text text

let makeAdaptiveText text = FraxineNode.AdaptiveText text

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


  member _.text(value: aval<string>) = makeAdaptiveText value

  member this.text(value: aval<int>) =
    value |> AVal.map (fun value -> $"{value}") |> this.text

[<Literal>]
let validCustomElementName =
  @"^([a-z]+[-._0-9]+([.0-9]|\w)+|\barticle\b|\baside\b|\bblockquote\b|\bbody\b|\bdiv\b|\bfooter\b|(h{1}[1-6]{1})|\bheader\b|\bmain\b|\bnav\b|\bp\b|\bsection\b|\bspan\b)$"

type FraxineShadowEngine() =

  let ceNameregex = new Regex(validCustomElementName)

  let forbiddenNames =
    set [
      "annotation-xml"
      "color-profile"
      "font-face"
      "font-face-src"
      "font-face-uri"
      "font-face-format"
      "font-face-name"
      "missing-glyph"
    ]

  member _.custom(tagname: string, children: FraxineNode seq) =
    if forbiddenNames |> Set.contains tagname then
      failwith "Invalid Element Name"

    if ceNameregex.IsMatch tagname then
      makeNode true tagname children
    else
      failwith "Invalid Element Name"


  member _.article(children) = makeNode true "article" children
  member this.article(text) = this.article [ makeText text ]

  member _.aside(children) = makeNode true "aside" children
  member this.aside(text) = this.aside [ makeText text ]

  member _.blockquote(children) = makeNode true "blockquote" children
  member this.blockquote(text) = this.blockquote [ makeText text ]

  member _.div(children) = makeNode true "div" children
  member this.div(text) = this.div [ makeText text ]

  member _.footer(children) = makeNode true "footer" children
  member this.footer(text) = this.footer [ makeText text ]

  member _.header(children) = makeNode true "header" children
  member this.header(text) = this.header [ makeText text ]

  member _.h1(children) = makeNode true "h1" children
  member this.h1(text) = this.h1 [ makeText text ]

  member _.h2(children) = makeNode true "h2" children
  member this.h2(text) = this.h2 [ makeText text ]

  member _.h3(children) = makeNode true "h3" children
  member this.h3(text) = this.h3 [ makeText text ]

  member _.h4(children) = makeNode true "h4" children
  member this.h4(text) = this.h4 [ makeText text ]

  member _.h5(children) = makeNode true "h5" children
  member this.h5(text) = this.h5 [ makeText text ]

  member _.h6(children) = makeNode true "h6" children
  member this.h6(text) = this.h6 [ makeText text ]

  member _.main(children) = makeNode true "main" children
  member this.main(text) = this.main [ makeText text ]

  member _.nav(children) = makeNode true "nav" children
  member this.nav(text) = this.nav [ makeText text ]

  member _.p(children) = makeNode true "p" children
  member this.p(text) = this.p [ makeText text ]

  member _.section(children) = makeNode true "section" children
  member this.section(text) = this.section [ makeText text ]

  member _.span(children) = makeNode true "span" children
  member this.span(text) = this.span [ makeText text ]


  member _.stylesheet(selector: string, properties: FraxineCssprop seq) =
    {
      selector = selector
      properties = properties
    }
    |> FraxineNode.Stylesheet

type FraxineAttributeEngine() =
  inherit AttrEngine<FraxineNode>(makeStrAttr, makeBoolAttr)

  member _.style(props: FraxineCssprop seq) : FraxineNode =
    FraxineNode.InlineStyle { properties = props }

  member _.isChecked(value: aval<bool>) : FraxineNode =
    FraxineNode.Attribute("checked", AdaptiveBool value)

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

  static member mount(target: Element, childContent) =
    setupNode target childContent

  static member mount(target: string, node) =
    let target = document.querySelector target |> Option.ofObj

    match target with
    | None -> eprintfn "Target not found"
    | Some target -> DOM.mount (target, node)
