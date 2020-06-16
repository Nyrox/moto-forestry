

type pass = {
    name: string;
}

type model = {
    passes: pass list;
    createDialog: string option;
}

type msg =
    | SetNewFileDialog of string option
    | CreatePass of string
    | DoNothing


let init () = 
    {
        passes=[];
        createDialog=None;
    }

let update model msg =
    match msg with
    | SetNewFileDialog msg -> {model with createDialog=msg}
    | CreatePass name -> 
        let pass = {name} in
        { model with createDialog=None; passes=pass::model.passes}
    | DoNothing -> model

open Tea.Html


let passItem pass =
    li [class' "pass-item"] [text pass.name]

let viewHeader model = 
    div [class' "header"] 
        [ h2 [] [text "Passes"]
        ; span [class' "button"; onClick @@ SetNewFileDialog (Some "")] @@ [text "+"] ]

let view model: (msg Vdom.t * msg Vdom.t list) =
    let createFileDialog = match model.createDialog with
    | None -> Vdom.noNode
    | Some v -> 
        let _ = [%raw {|
        setTimeout(() => document.querySelector("#pass-list-create-dialog-field").focus(), 1)
        |}] in
        form [Tea.Html.onWithOptions ~key:v "submit" {defaultOptions with preventDefault=true} @@ Tea.Json.Decoder.succeed (CreatePass v)] 
            [input'
                [ type' "text"
                ; classList ["pass-item", true; "create-dialog", true]
                ; id "pass-list-create-dialog-field"
                ; Tea.Html.onBlur @@ SetNewFileDialog None
                ; Tea.Html.value v
                ; Tea.Html.onInput (fun s -> SetNewFileDialog (Some s))
                ] []]
    in
    (
        div [Tea.Html.classList [("pass-list", true); ("is-experiencing-input", Option.isSome model.createDialog)]]
            [ viewHeader model
            ; div [class' "pass-items"] @@ List.concat 
                [ List.map passItem @@ List.rev model.passes
                ; [ createFileDialog ]
                ; ]
            ],
        [noNode]
    )