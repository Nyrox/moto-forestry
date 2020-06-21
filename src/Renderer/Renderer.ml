let (>>) f g = (fun a -> g (f a))
let (<<) f g = (fun a -> f (g a))


open Tea.App
open Tea.Html

open Utils

open AppDomain

let jsonTest = 
    let _testProject = AppDomain.createProject () in
    AppDomain.Encode.project 
        {_testProject with passes=[{name="TestPass"; vertexInput=AppDomain.mesh @@ Path.resolve ("./teapot.obj")}]}
        |> Json.stringify
        |> Node.Fs.writeFileAsUtf8Sync "rad.json";;


let customSelect (value: string) (options: (string * 'a) list): 'a Vdom.t =
    let createOption (label, msg) =
        let onClick e =
            (* Yes. I know. *)
            let removeActive = [%raw {|
                (e) => e.target.parentElement.parentElement.classList.remove("active")
            |}] in
            removeActive e;
            Some msg
        in
        li [onCB "click" "" onClick] [text label]
    in
    let setActive e =
        let impl = [%raw {|
            (e) => e.target.classList.add("active")
        |}] in
        impl e;
        None
    in
        div [class' "custom-select"; onCB "click" "" setActive]
            [ text value
            ; ul [class' "options"] @@ List.map createOption options
            ];


module PassView = struct
    type tab =
    | Input
    | FragmentShader

    type model = {
        tab: tab;
        editor: MonacoEditor.model;
    }

    type msg =
    | Tab of tab
    | SetVertexInputData of AppDomain.vertexInputData
    | EditorMsg of MonacoEditor.msg
    [@@bs.deriving accessors]

    type intent =
    | UpdatePassData of AppDomain.pass
    | DoNothing

    let init () = 
        { tab=Input; editor=MonacoEditor.init "<loading...>" }
    
    let update model pass msg =
        match msg with
        | Tab t -> {model with tab=t}, DoNothing
        | SetVertexInputData v ->
            model, UpdatePassData {pass with vertexInput=v}
        | EditorMsg msg ->
            {model with editor=MonacoEditor.update model.editor msg}, DoNothing
        
    let vertexInputBlock (pass: pass) =
        let label: string = 
            match pass.vertexInput with
            | Mesh _ -> "Mesh"
            | FullScreenPass -> "Fullscreen"
            | _ -> ""
        in
        fieldset [] @@
            [ customSelect label
                [ "Mesh", setVertexInputData @@ AppDomain.mesh @@ Utils.Path.absolute ""
                ; "Fullscreen", setVertexInputData AppDomain.FullScreenPass                        
                ]
            ]

    let viewVertexDataBlock model (pass: pass) =
        div [class' "block"]
            [ h2 [] [text "Vertex Data"]
            ; vertexInputBlock pass
            ; div 
                [ styles ["width", "200px"; "height", "50px"; "background", "blue"]
                ; onCB "dragover" "" (fun e ->
                    [%raw {| e.preventDefault() |}];
                    [%raw {| e.dataTransfer.dropEffect = "link" |}];
                None)
                ; onCB "drop" "" (fun e ->
                    [%raw {| e.preventDefault() |}];
                    let elemType = [%raw {| e.dataTransfer.getData("application/elem-type") |}] in
                    match elemType with
                    | "file" -> Some (setVertexInputData @@ AppDomain.mesh @@ Path.absolute "your mom")
                    | _ -> None
                )
                ]
                []
            ]

    let viewUniformDataBlock model pass =
        div [class' "block"]
            [ h2 [] [text "Uniform Data"]
            ]

    let viewSamplerDataBlock model pass =
        div [class' "block"]
            [ h2 [] [text "Samplers"]
            ]

    let view model pass =
        let panelTabButton tab =
            let isActive = tab == model.tab in
            let label = match tab with
                | Input -> "Input"
                | FragmentShader -> "Fragment Shader"
            in
            button [onClick @@ Tab tab; classList [("active", isActive)]] [text label]
        in
        let panelNavigation =
            nav [class' "panel-navigation"]
            [ panelTabButton Input
            ; panelTabButton FragmentShader ]
        in
        let panelContent = div [class' "panel"] @@ match model.tab with
            | Input ->
                [ viewVertexDataBlock model pass
                ; viewUniformDataBlock model pass
                ; viewSamplerDataBlock model pass
                ]
            | FragmentShader ->
                [ Vdom.map (fun v -> EditorMsg v) @@ MonacoEditor.view model.editor ]
        in
            [panelNavigation; panelContent]
end




type mainWindowState =
    | Empty
    | PassWindow of string * PassView.model
    [@@bs.deriving accessors]

type model = {
    fileTree: FileTree.model;
    global: Global.model;
    project: AppDomain.Project.t;
    passList: PassList.model;
    mainWindow: mainWindowState;
}

type msg = 
    | FileTreeMsg of FileTree.msg
    | PassListMsg of PassList.msg
    | GlobalMsg of Global.msg
    | PassViewMsg of PassView.msg
    | NoOp
    [@@bs.deriving accessors]

let projectPath = "./TestProject/"
let getManifestPath () =
    Node.Path.resolve projectPath "project-manifest.json"

let loadProject () =
    let manifestPath = getManifestPath() in
    if Node.Fs.existsSync (manifestPath) then
        Node.Fs.readFileAsUtf8Sync (manifestPath)
            |> Json.parseOrRaise
            |> AppDomain.Decode.project
    else
        AppDomain.createProject ()


let init () =
    let (fileTree, cmd) = FileTree.init @@ Path.resolve projectPath in
    ({
        fileTree=fileTree;
        global=Global.init ();
        passList=PassList.init ();
        mainWindow=Empty;
        project=loadProject ();
    }, Tea.Cmd.map (fun v -> FileTreeMsg v) cmd)

let loadFile: (string -> string) = [%raw {|
function (path) {
    return require("fs").readFileSync(path, "utf-8")
}
|}]

let writeFile: (string -> string -> unit) = [%raw {|
function (path, content) {
    require("fs").writeFile(path, content, _ => undefined)
}
|}]

let whackOutModel model =
    let path = getManifestPath () in
    let fileContents =
        AppDomain.Encode.project model.project
        |> Json.stringify
    in
        writeFile path fileContents

let getSelectedPass model =
    match model.mainWindow with
    | PassWindow (n, _) -> Some (List.find (fun (p: AppDomain.pass) -> p.name == n) model.project.passes)
    | _ -> None


let update (model: model) (msg: msg) =
    whackOutModel model;

    match msg with
    | FileTreeMsg msg ->
        let fileTree = FileTree.update model.fileTree msg in
        {model with fileTree=fileTree}
    | PassListMsg msg ->
        let passList, intent = PassList.update model.passList msg in
        match intent with 
        | OpenPass p ->
            { model with passList=passList; mainWindow=passWindow p @@ PassView.init ()}
        | CreatePass p ->
            { model with passList=passList; mainWindow=passWindow p @@ PassView.init (); project = AppDomain.Project.createPass model.project p}
        | DoNothing -> 
            { model with passList=passList };
        ;
    | PassViewMsg msg ->
        match model.mainWindow with
        | PassWindow (s, m) ->
            let passView, intent = PassView.update m (Option.unwrap @@ getSelectedPass model) msg
            in match intent with
            | UpdatePassData p ->
                {model with mainWindow=PassWindow (s, passView); project=AppDomain.Project.updatePass model.project s p}
            | DoNothing ->
                {model with mainWindow=PassWindow (s, passView)};
            ;
        | _ -> failwith "wrong state";
        ;
    | GlobalMsg msg -> 
        let globalState = Global.update model.global msg in
        {model with global=globalState}
    | NoOp -> model





let sidebar fileTree passList =
    div [class' "sidebar"]
        [ Vdom.map (fun v -> PassListMsg v) @@ passList
        ; Vdom.map (fun v -> FileTreeMsg v) @@ fileTree
        ]






let view (model: model)=
    let fileTree, globalItems = FileTree.view model.fileTree in
    let passList, globalItems = 
        PassList.view model.passList model.project.passes (Option.map (fun (p: AppDomain.pass) -> p.name) @@ getSelectedPass model)
        |> (fun (a, b) -> a, List.concat [List.map (Vdom.map (fun v -> PassListMsg v)) b; List.map (Vdom.map (fun v -> FileTreeMsg v)) globalItems])
    in
    Vdom.node "transparent" [Vdom.style "display" "contents"]
        @@ List.concat [
            [sidebar fileTree passList];
            [div [class' "main-view"]
                (match model.mainWindow with
                | Empty -> []
                | PassWindow (p, _model) -> 
                    List.map (Vdom.map passViewMsg) @@ PassView.view _model @@ (Option.unwrap @@ getSelectedPass model)
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
        update=(fun model msg -> [%raw {| window.__elmModel = model|}]; update model msg, Tea.Cmd.none);
        view;
        subscriptions;
    } (Web_document.getElementById "app") ()
    

