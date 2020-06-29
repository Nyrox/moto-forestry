let (>>) f g = (fun a -> g (f a))
let (<<) f g = (fun a -> f (g a))


open Tea.App
open Tea.Html

open Utils

open AppDomain


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
            ; img [src @@ Utils.getStaticAssetPath "icons/select_expand_arrow.svg"] []
            ; ul [class' "options"] @@ List.map createOption options
            ];
;;


let unsafe_getGlobalFileTree: (unit -> FileTree.node) = [%raw {|
function() {
    return window.__elmModel.fileTree.tree;
}
|}]

let unsafe_getGlobalProject: (unit -> AppDomain.Project.t) = [%raw {|
function() {
    return window.__elmModel.project;
}
|}]

let unsafe_getProjectBasePath: (unit -> Path.t) = [%raw {|
function() {
    return window.__elmModel.basePath;
}
|}]

module PassView = struct
    type tab =
    | Input
    | FragmentShader

    type model = {
        tab: tab;
        editor: MonacoEditor.model option;
        fragmentShaderFileSearchBox: string;
    }

    type msg =
    | Tab of tab
    | SetVertexInputData of AppDomain.vertexInputData
    | SetFragmentShaderFile of string
    | SetFragmentShaderFileSearchBox of string
    | EditorMsg of MonacoEditor.msg
    [@@bs.deriving accessors]

    type intent =
    | UpdatePassData of AppDomain.pass
    | DoNothing

    let init () = 
        { tab=Input; editor=Some (MonacoEditor.init "<loading...>"); fragmentShaderFileSearchBox="" }
    
    let update model (pass: Pass.t) msg =
        match msg with
        | Tab t -> 
            match t with
            | FragmentShader -> 
                let editor = Option.map MonacoEditor.loadFile pass.fragShader in
                {model with tab=t; editor=editor}, DoNothing
            | _ -> {model with tab=t}, DoNothing;
        ;
        | SetFragmentShaderFileSearchBox v -> 
            {model with fragmentShaderFileSearchBox=v}, DoNothing
        | SetVertexInputData v ->
            model, UpdatePassData {pass with vertexInput=v}
        | SetFragmentShaderFile v ->
            let editor = MonacoEditor.loadFile @@ Utils.Path.absolute v in
            {model with editor=Some editor}, UpdatePassData {pass with fragShader=Some (Path.absolute v)}
        | EditorMsg msg ->
            {model with editor=Option.map (fun m -> MonacoEditor.update m msg) model.editor}, DoNothing
        
    let vertexInputBlock (pass: pass) =
        let value: string = 
            match pass.vertexInput with
            | Mesh _ -> "Mesh"
            | FullScreenPass -> "Fullscreen"
            | _ -> ""
        in
        fieldset [] @@
            [ label [] [text "Vertex Input Type"] 
            ; customSelect value
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

    let customSearchBox current setValue getOptions abort =
        let createOption (label, msg) =
            li ~key:label [onCB "click" "" (fun e -> 
                let collapse = [%raw {|
                    (e) => e.target.parentElement.parentElement.classList.remove("expand")
                |}] in
                Js.log msg;
                collapse e;
                Some msg)] [text label]
        in
        let setActive e =
            let impl = [%raw {|
                (e) => e.target.parentElement.classList.add("expand")
            |}] in
            impl e;
            None
        in
        div [class' "custom-searchbox"]
            [input'
                [ type' "text"
                ; classList ["pass-item", true; "create-dialog", true]
                ; id "custom-search-input"
                ; onCB "focus" "" setActive
                ; value current
                ; onInput setValue
                ] []
            ; div [class' "options-list"] @@ List.map createOption (getOptions current)
            ]
        
    let viewFragmentShaderBlock model (pass: pass) =
        div [class' "block"]
            [ h2 [][text "Fragment Shader"]
            ; fieldset [] @@
                [ label [] [text "Fragment Shader File"] 
                ; customSearchBox
                    model.fragmentShaderFileSearchBox 
                    (fun v -> SetFragmentShaderFileSearchBox v)
                    (fun v -> 
                        let files = FileTree.findFiles (fun f ->
                            Option.isSome @@ List.find_opt
                                (fun v -> v == Path.extname f)
                                [".sl"; ".glsl"]
                            && Js_string.includes v @@ Path.asString f
                        ) @@ unsafe_getGlobalFileTree ()
                        in files |>
                            List.map (fun v -> 
                                Path.relative (unsafe_getProjectBasePath ()) v,
                                SetFragmentShaderFile (Path.asString v))
                    )
                    (SetFragmentShaderFileSearchBox (Option.orDefault "" @@ Option.map Path.asString pass.fragShader))
                ]
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
        let panelContent = match model.tab with
            | Input ->
                div [class' "panel"]
                    [ viewVertexDataBlock model pass
                    ; viewUniformDataBlock model pass
                    ; viewSamplerDataBlock model pass
                    ]
            | FragmentShader ->
                div [class' "panel"]
                    [ viewFragmentShaderBlock model pass
                    ; match model.editor with
                    | Some m -> Vdom.map (fun v -> EditorMsg v) @@ MonacoEditor.view m
                    | None -> noNode
                    ]
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
    basePath: Path.t;
}



type msg = 
    | FileTreeMsg of FileTree.msg
    | PassListMsg of PassList.msg
    | GlobalMsg of Global.msg
    | PassViewMsg of PassView.msg
    | NoOp
    [@@bs.deriving accessors]


let getManifestPath projectPath =
    Node.Path.resolve projectPath "project-manifest.json"

let loadProject projectPath =
    let manifestPath = getManifestPath projectPath in
    if Node.Fs.existsSync (manifestPath) then
        Node.Fs.readFileAsUtf8Sync (manifestPath)
            |> Json.parseOrRaise
            |> AppDomain.Decode.project
    else
        AppDomain.createProject ()


let init () =
    let projectPath = "./TestProject" in
    let (fileTree, cmd) = FileTree.init @@ Path.resolve projectPath in
    ({
        fileTree=fileTree;
        global=Global.init ();
        passList=PassList.init ();
        mainWindow=Empty;
        project=loadProject projectPath;
        basePath=Path.resolve projectPath;
    }, Tea.Cmd.map (fun v -> FileTreeMsg v) cmd)



let whackOutModel model =
    let path = getManifestPath @@ Path.asString model.basePath in
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
    

