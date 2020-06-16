let (>>) f g = (fun a -> g (f a))
let (<<) f g = (fun a -> f (g a))

open Tea
open Tea.App
open Tea.Html


type model = {
    fileTree: FileTree.model;
    global: Global.model;
    editor: MonacoEditor.model;
    passes: PassList.model;
}

type msg = 
    | FileTreeMsg of FileTree.msg
    | PassListMsg of PassList.msg
    | GlobalMsg of Global.msg
    | EditorMsg of MonacoEditor.msg
    | NoOp
    [@@bs.deriving accessors]

let init () =
    let (fileTree, cmd) = FileTree.init @@ FileTree.Path.resolve "./src" in
    ({
        fileTree=fileTree;
        global=Global.init ();
        passes=PassList.init ();
        editor=MonacoEditor.init "Ready to roll";
    }, Cmd.map (fun v -> FileTreeMsg v) cmd)

let loadFile: (string -> string) = [%raw {|
function (path) {
    return require("fs").readFileSync(path, "utf-8")
}
|}]

let update (model: model) (msg: msg) =
    match msg with
    | FileTreeMsg msg ->
        let fileTree, appCmds, intent = FileTree.update model.fileTree msg in
        match intent with
        | FileTree.OpenFile file ->
            {model with editor={value=loadFile @@ FileTree.Path.asString file}}, Cmd.map (fun v -> GlobalMsg v) appCmds
        | FileTree.DoNothing ->
            {model with fileTree=fileTree}, Cmd.map (fun v -> GlobalMsg v) appCmds;
        ;
    | PassListMsg msg ->
        let passes = PassList.update model.passes msg in
        { model with passes=passes }, Cmd.none
    | GlobalMsg msg -> 
        let globalState = Global.update model.global msg in
        {model with global=globalState}, Cmd.none
    | EditorMsg msg ->
        {model with editor=MonacoEditor.update model.editor msg}, Cmd.none
    | NoOp -> model, Cmd.none

let sidebar fileTree passList =
    div [class' "sidebar"]
        [ Vdom.map (fun v -> PassListMsg v) @@ passList
        ; Vdom.map (fun v -> FileTreeMsg v) @@ fileTree
        ]

let view (model: model)=
    let fileTree, globalItems = FileTree.view model.fileTree in
    let passList, globalItems = 
        PassList.view model.passes
        |> (fun (a, b) -> a, List.concat [List.map (Vdom.map (fun v -> PassListMsg v)) b; List.map (Vdom.map (fun v -> FileTreeMsg v)) globalItems])
    in
    Vdom.node "transparent" [Vdom.style "display" "contents"]
        @@ List.concat [
            [sidebar fileTree passList];
            [div [class' "main-view"]
                [ Vdom.map (fun v -> EditorMsg v) @@ MonacoEditor.view model.editor;
                ]];            
            List.map (Vdom.map (fun v -> GlobalMsg v)) @@ Global.view model.global;
            globalItems;
        ]

let subscriptions model =
    Tea.Sub.map (fun m -> FileTreeMsg m) @@ FileTree.subscriptions model.fileTree

let rendererEntry () =
    standardProgram {
        init=init;
        update;
        view;
        subscriptions;
    } (Web_document.getElementById "app") ()
    

