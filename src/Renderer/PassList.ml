

type pass = {
    name: string;
}

type model = {
    passes: pass list
}

type msg =
    | DoNothing


let init () = 
    {
        passes=[]
    }

let update model msg =
    match msg with
    | DoNothing -> model


open Tea.Html


let passItem pass =
    li [class' "pass-item"] [text pass.name]

let view model: (msg Vdom.t * msg Vdom.t list) =
    (
        div [class' "pass-list"]
            [ h2 [] [text "Passes"]
            ; div [class' "pass-items"] @@ List.map passItem model.passes
            ],
        [noNode]
    )