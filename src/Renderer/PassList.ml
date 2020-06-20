

type model = {
    createDialog: string option;
}

type intent =
    | OpenPass of string
    | CreatePass of string
    | DoNothing
    [@@bs.deriving accessors]

type msg =
    | SetCreateDialog of string option
    | SubmitCreateDialog of string
    | DoubleClickPass of string
    [@@bs.deriving accessors]

let init () = 
    {
        createDialog=None;
    }

let update model msg =
    match msg with
    | SetCreateDialog msg -> {model with createDialog=msg}, DoNothing
    | SubmitCreateDialog name ->
        if Option.isSome model.createDialog && String.length name > 0 then
            { model with createDialog=None}, CreatePass name
        else
            { model with createDialog=None; }, DoNothing
    | DoubleClickPass p -> model, OpenPass p

open Tea.Html


let passItem (pass: AppDomain.pass) active =
    li 
        [ classList ["pass-item", true; "active", active]
        ; onDoubleClick @@ DoubleClickPass pass.name]
        [text pass.name]

let viewHeader model = 
    div [class' "header"] 
        [ h2 [] [text "Passes"]
        ; span [class' "button"; onClick @@ SetCreateDialog (Some "")] @@ [text "+"] ]

let view model (passes: AppDomain.pass list) (active: string option): (msg Vdom.t * msg Vdom.t list) =
    let createFileDialog = match model.createDialog with
    | None -> Vdom.noNode
    | Some v -> 
        let _ = [%raw {|
        setTimeout(() => document.querySelector("#pass-list-create-dialog-field").focus(), 1)
        |}] in
        form [Tea.Html.onWithOptions ~key:v "submit" {defaultOptions with preventDefault=true} @@ Tea.Json.Decoder.succeed (submitCreateDialog v)] 
            [input'
                [ type' "text"
                ; classList ["pass-item", true; "create-dialog", true]
                ; id "pass-list-create-dialog-field"
                ; Tea.Html.onBlur @@ submitCreateDialog v
                ; Tea.Html.value v
                ; Tea.Html.onInput (fun s -> SetCreateDialog (Some s))
                ] []]
    in
    (
        div [Tea.Html.classList [("pass-list", true); ("is-experiencing-input", Option.isSome model.createDialog)]]
            [ viewHeader model
            ; div [class' "pass-items"] @@ List.concat 
                [ List.map (fun p -> passItem p (Some p.name == active)) @@ List.rev passes
                ; [ createFileDialog ]
                ; ]
            ],
        [noNode]
    )