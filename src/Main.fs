module Main

open FSharp.Data.Adaptive
open Browser.Dom
open Browser.Types

open FraxineEngine

let counter (initial: int) =
  let counter = cval (initial)

  Html.section [
    Html.button [
      Attr.style ([ Css.color "blue" ])
      Event.onClick (fun _ ->
        transact (fun () -> counter.Value <- counter.Value + 1))
      Html.text "Increment"
    ]
    Html.text counter
    Html.button [
      Event.onClick (fun _ ->
        transact (fun () -> counter.Value <- counter.Value - 1))
      Shadow.span [
        Html.text "Decrement"
        Shadow.stylesheet (":host", [ Css.color "rebeccapurple" ])
      ]
    ]
  ]

let checkUncheck (show: cval<bool>) =
  let message =
    show
    |> AVal.map (fun show ->
      if show then Html.text "Showing!" else Html.text "hiding!")

  Shadow.section [
    Shadow.stylesheet (":host", [ Css.color "orange" ])
    Shadow.stylesheet (":nth-child(even)", [ Css.color "green" ])


    Html.label [
      Html.text "I should change every 5 secods or when required!!"
      Html.input [
        Attr.isChecked show
        Attr.typeCheckbox
        Attr.readOnly true
        Attr.disabled true
      ]
    ]

    Html.label [
      Html.text "Change me!"
      Html.input [
        Attr.typeCheckbox
        Attr.isChecked show
        Event.onCheckedChange (fun event ->
          transact (fun () -> show.Value <- event))
      ]
    ]

    Html.adaptive message
  ]

let view =
  let show = cval (false)

  window.setInterval (
    (fun () -> transact (fun () -> show.Value <- not show.Value)),
    5000,
    [||]
  )
  |> ignore

  Html.article [
    Attr.id "olv"
    Html.text "Hello there!"
    Attr.style ([ Css.color "red" ])
    counter 0
    checkUncheck show
  ]

DOM.mount ("#fable-app", view)
