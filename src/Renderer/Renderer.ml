let (>>) f g = (fun a -> g (f a))
let (<<) f g = (fun a -> f (g a))

open Tea
open Tea.App
open Tea.Html


type tab =
    | Input
    | FragmentShader

type mainWindowState =
    | Empty
    | PassWindow of string * tab
    [@@bs.deriving accessors]

type model = {
    fileTree: FileTree.model;
    global: Global.model;
    editor: MonacoEditor.model;
    passes: PassList.model;
    mainWindow: mainWindowState;
}

type msg = 
    | FileTreeMsg of FileTree.msg
    | PassListMsg of PassList.msg
    | GlobalMsg of Global.msg
    | EditorMsg of MonacoEditor.msg
    | TabMainPanelMsg of tab
    | NoOp
    [@@bs.deriving accessors]

let init () =
    let (fileTree, cmd) = FileTree.init @@ FileTree.Path.resolve "./src" in
    ({
        fileTree=fileTree;
        global=Global.init ();
        passes=PassList.init ();
        editor=MonacoEditor.init "Ready to roll";
        mainWindow=Empty
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
        let passes, intent = PassList.update model.passes msg in
        match intent with 
        | OpenPass p ->
            { model with passes=passes; mainWindow=passWindow p Input }, Cmd.none
        | DoNothing -> 
            { model with passes=passes }, Cmd.none;
        ;
    | GlobalMsg msg -> 
        let globalState = Global.update model.global msg in
        {model with global=globalState}, Cmd.none
    | EditorMsg msg ->
        {model with editor=MonacoEditor.update model.editor msg}, Cmd.none
    | TabMainPanelMsg tab ->
        match model.mainWindow with
        | PassWindow (pass, _) -> { model with mainWindow=passWindow pass tab }, Cmd.none
        | _ -> raise @@ Failure "Unexpected model state for message";
        ;
    | NoOp -> model, Cmd.none


let sidebar fileTree passList =
    div [class' "sidebar"]
        [ Vdom.map (fun v -> PassListMsg v) @@ passList
        ; Vdom.map (fun v -> FileTreeMsg v) @@ fileTree
        ]


let viewVertexDataBlock model pass =
    div [class' "block"]
        [ h2 [] [text "Vertex Data"]
        ]

let viewUniformDataBlock model pass =
    div [class' "block"]
        [ h2 [] [text "Uniform Data"]
        ]

let viewSamplerDataBlock model pass =
    div [class' "block"]
        [ h2 [] [text "Samplers"]
        ]

let viewPassWindow model pass activeTab =
    let panelTabButton tab =
        let isActive = tab == activeTab in
        let label = match tab with
            | Input -> "Input"
            | FragmentShader -> "Fragment Shader"
        in
        button [onClick @@ tabMainPanelMsg tab; classList [("active", isActive)]] [text label]
    in
    let panelNavigation =
        nav [class' "panel-navigation"]
        [ panelTabButton Input
        ; panelTabButton FragmentShader ]
    in
    let panelContent = div [class' "panel"] @@ match activeTab with
        | Input ->
            [ viewVertexDataBlock model pass
            ; viewUniformDataBlock model pass
            ; viewSamplerDataBlock model pass
            ]
        | FragmentShader ->
            [ Vdom.map (fun v -> EditorMsg v) @@ MonacoEditor.view model.editor ]
    in
        [panelNavigation; panelContent]


let getSelectedPass model =
    match model.mainWindow with
    | PassWindow (n, _) -> Some n
    | _ -> None

let view (model: model)=
    let fileTree, globalItems = FileTree.view model.fileTree in
    let passList, globalItems = 
        PassList.view model.passes @@ getSelectedPass model
        |> (fun (a, b) -> a, List.concat [List.map (Vdom.map (fun v -> PassListMsg v)) b; List.map (Vdom.map (fun v -> FileTreeMsg v)) globalItems])
    in
    Vdom.node "transparent" [Vdom.style "display" "contents"]
        @@ List.concat [
            [sidebar fileTree passList];
            [div [class' "main-view"]
                (match model.mainWindow with
                | Empty -> []
                | PassWindow (p, t) -> 
                    viewPassWindow model p t
                )
            ];
            List.map (Vdom.map (fun v -> GlobalMsg v)) @@ Global.view model.global;
            globalItems;
        ]

let subscriptions model =
    Tea.Sub.batch
    [ Tea.Sub.map (fun m -> FileTreeMsg m) @@ FileTree.subscriptions model.fileTree
    ; ]

let rendererEntry () =
    standardProgram {
        init=init;
        update;
        view;
        subscriptions;
    } (Web_document.getElementById "app") ()
    

