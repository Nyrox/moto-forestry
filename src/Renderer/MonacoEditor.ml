open Utils

type model = {
    value: string;
}

type msg = 
    | CodeChanged of string

let init code =
    {value=code}

let loadFile path =
    {value=Utils.loadFileSync @@ Path.asString path}

let update model msg =
    match msg with
    | CodeChanged s -> {model with value=s}

open Tea.Html
open Tea.Json
let view (model: model) =
    Vdom.node ~key:model.value "monaco-editor"
        [ Vdom.prop "_editorValue" model.value
        ; Vdom.onCB "editorValueChanged" "" (fun e ->
            let mapper = Decoder.map (fun v -> CodeChanged v) (Decoder.at ["target"; "_editorValue"] Decoder.string) in
            Decoder.decodeEvent mapper e |> Tea.Result.result_to_option
        )] []