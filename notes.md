```fsharp
module Main

open Browser.Dom
open Feliz
open Browser.Types

open FSharp.Data.Adaptive
open Fable.Core.JsInterop

type FraxineAttr =
    | Bool of bool
    | String of string

type FraxineNode =
    | Text of Text
    | HTMLElement of HTMLElement
    | DocumentFragment of DocumentFragment
    | Attr of string * FraxineAttr
    | Event of name: string * handler: (Event -> unit)

let append (element: Element, node) = element.appendChild node |> ignore

let setAttribute (element: Element, attribute) = element.setAttribute attribute

let makeNode tag (children: FraxineNode seq) =
    let el = document.createElement tag

    for child in children do
        match child with
        | Text text -> append (el, text)
        | HTMLElement child -> append (el, child)
        | DocumentFragment fragment -> append (el, fragment)
        | Attr(name, Bool true) -> setAttribute (el, (name, ""))
        | Attr(name, Bool false) -> el.removeAttribute (name)
        | Attr(name, String value) -> setAttribute (el, (name, value))
        | Event(name, handler) ->
            let name = name.ToLowerInvariant()
            el.addEventListener (name, handler)

    el |> HTMLElement

let makeText text =
    let el = document.createTextNode text
    el |> Text

let makeFragment () =
    DocumentFragment(document.createDocumentFragment ())

let makeStrAttr name value = Attr(name, String value)
let makeBoolAttr name value = Attr(name, Bool value)
let makeEvent name handler = Event(name, handler)


let Html = HtmlEngine(makeNode, makeText, makeFragment)

let Attr = AttrEngine(makeStrAttr, makeBoolAttr)

let Ev = EventEngine(makeEvent)

type DOM =

    static member mount(target: Element, node) =
        match node with
        | Text value -> target.appendChild value |> ignore
        | HTMLElement value -> target.appendChild value |> ignore
        | DocumentFragment value -> target.appendChild value |> ignore
        | Attr(k, v) ->
            match v with
            | Bool value ->
                if value then
                    target.setAttribute (k, "")
            | String value -> target.setAttribute (k, value)
        | Event(name, handler) -> target.addEventListener (name, handler)

    static member mount(target: string, node) =
        let target = document.querySelector target |> Option.ofObj

        match target with
        | None -> eprintfn "Target not found"
        | Some target -> DOM.mount (target, node)

let counter (initial: int) =
    let counter = cval (initial)
    counter.AddCallback(fun v -> printfn $"{v}") |> ignore

    Html.section
        [ Html.button
              [ Ev.onClick (fun _ -> transact (fun () -> counter.Value <- counter.Value + 1))
                Html.text "Increment" ]
          Html.text counter.Value
          Html.button
              [ Ev.onClick (fun _ -> transact (fun () -> counter.Value <- counter.Value - 1))
                Html.text "Decrement" ] ]

let view =
    Html.article [ Attr.id "olv"; Attr.isOpen true; Html.text "Hello there!"; counter 0 ]

DOM.mount ("#fable-app", view)

```
