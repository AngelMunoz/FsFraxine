module Main

open FSharp.Data.Adaptive
open FraxineEngine


let counter (initial: int) =
    let counter = cval (initial)
    counter.AddCallback(fun v -> printfn $"{v}") |> ignore

    Html.section
        [ Html.button
              [ Attr.style ([ Css.color "blue" ])
                Event.onClick (fun _ -> transact (fun () -> counter.Value <- counter.Value + 1))
                Html.text "Increment" ]
          Html.text counter.Value
          Html.button
              [ Event.onClick (fun _ -> transact (fun () -> counter.Value <- counter.Value - 1))
                Shadow.span
                    [ Html.text "Decrement"
                      Shadow.stylesheet (":host", [ Css.color "rebeccapurple" ]) ] ] ]

let view =
    Html.article
        [ Attr.id "olv"
          Attr.isOpen true
          Html.text "Hello there!"
          Attr.style ([ Css.color "red" ])
          counter 0 ]

DOM.mount ("#fable-app", view)
