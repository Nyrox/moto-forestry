

module Path = struct
    type t = {repr: string}
    
    let absolute path =
        {repr=path}

    let resolve path =
        absolute @@ Node.Path.resolve path "."

    let basename file =
        Node.Path.basename file.repr
    
    let dirname file =
        Node.Path.dirname file.repr

    let asString file =
        file.repr

    let extend path segment =
        absolute @@ Node.Path.resolve path.repr segment
end

type node =
    | File of Path.t
    | Directory of directory
and directory = Path.t * node list * bool



let readdir: (string -> (string Js.Array.t) Js.Promise.t) = [%raw {|
function (path) {
    let __impl = require("util").promisify(require("fs").readdir)
    return __impl(path)
}
|}]

[%%raw {|
let __impl_stat = require("util").promisify(require("fs").stat);
|}]

let dirtype_r: (string -> string Js.Promise.t) = [%raw {|
async function dirtype(path) {
    let __stat = await __impl_stat(path)
    if (__stat.isDirectory()) {
        return "directory"
    }
    else if (__stat.isFile()) {
        return "file"
    }
    else {
        return "other"
    }
}
|}]

module DirType = struct
    type dirtype = Directory | File | Other;;
end

let getDirtype path =
    dirtype_r path
    |> Js.Promise.then_ (fun s -> Js.Promise.resolve @@
        if s == "directory" then DirType.Directory
        else if s == "file" then DirType.File
        else DirType.Other
    )

let mapFileToStats f = 
    getDirtype @@ f
    |> Js.Promise.then_ (fun stats -> Js.Promise.resolve stats);;


let (>>) f g = (fun a -> g (f a))
let (<<) f g = (fun a -> f (g a))

let choose (l: 'a option list) =
    List.concat @@ List.map (function | None -> [] | Some x -> [x]) l

module Async = struct 
    include Js.Promise
    let let_ a b = Js.Promise.then_ b a
end


open Js.Promise

let rec buildTree (path: Path.t) =
    let%Async f = readdir @@ Path.asString path in
    let%Async fileStats = Js.Promise.all @@ Array.map (fun file -> 
            let path = Path.extend path file in
            let%Async stat = mapFileToStats @@ Path.asString path in
            resolve (path, stat)
        ) f
    in
    let%Async childNodes=
        let%Async nodes = fileStats |> Array.map (fun (path, stat) -> 
            match stat with
            | DirType.File -> resolve  (Some (File path))
            | DirType.Directory -> 
                let%Async tree = buildTree path in
                resolve @@ Some tree
            | DirType.Other -> resolve @@ None
        ) |> all in
        resolve @@ choose @@ Array.to_list nodes
    in
    resolve @@ Directory (path, childNodes, false)

open Tea.Html



type msg =
    | RebuiltTree of node
    | CreateContextMenu of msg ContextMenu.menu
    | DestroyContextMenu of msg option
    | DoubleClickFile of Path.t

type intent =
    | OpenFile of Path.t
    | DoNothing

type model =
    { base_path: Path.t;
      tree: node;
      contextMenu: msg ContextMenu.menu option;
    }

let init path =
    let tree = Directory (Path.absolute "loading", [], false) in
    let baseModel = { base_path = path;
      tree = tree;
      contextMenu = None;
    } in
    (baseModel, Tea_promise.result (buildTree path) (fun v -> RebuiltTree (match v with
        | Ok t -> t
        | Error s -> Js.log s; Directory (path, [], false)
    )))


let rec update model msg =
    Js.log model;
    match msg with
    | RebuiltTree node -> { model with tree=node }, Tea.Cmd.none, DoNothing
    | CreateContextMenu m -> Js.log m; { model with contextMenu=Some m }, Tea.Cmd.none, DoNothing
    | DestroyContextMenu msg ->
        let _model, cmd, intent = match msg with
        | Some msg -> update model msg
        | None -> model, Tea.Cmd.none, DoNothing
        in
        { _model with contextMenu=None }, cmd, intent
    | DoubleClickFile file -> model, Tea.Cmd.none, OpenFile file

let dirEntry replace (name, files, hidden): msg Vdom.t =
    let contextMenu: msg ContextMenu.menuItem list = [
        ("Open File", DestroyContextMenu (Some (RebuiltTree (replace @@ Directory (name, files, not hidden)))))
    ] in
    let srcString = Utils.getStaticAssetPath @@ if hidden then "icons/directory_open.svg" else "icons/directory.svg" in
    let icon = img [src srcString] [] in
    ContextMenu.contextMenuContainer (CreateContextMenu contextMenu) li [
        onClick @@ RebuiltTree (replace (Directory (name, files, not hidden)));
        class' (if hidden then "directory hidden" else "directory");
        ] [icon; text @@ Path.basename name]

let rec treeNode replace node = match node with
    | File name -> [ li [onDoubleClick @@ DoubleClickFile name] [ text @@ Path.basename name ] ]
    | Directory (name, files, hidden) ->
        let childNodes = List.mapi (fun i outerNode -> 
            treeNode (fun replaceWith ->
                replace @@ Directory (name, List.mapi (fun j node -> if i == j then replaceWith else node) files, hidden)
            ) outerNode
        ) files in
        [ dirEntry replace (name, files, hidden)
        ; ul [class' "file-list"] @@ List.flatten childNodes
        ; ]

let view model: (msg Vdom.t * msg Vdom.t list) =
    let ft = ul [class' "file-tree"] @@ treeNode (fun v -> v) @@ model.tree in
    (
        ft,
        [
            Option.orDefault Vdom.noNode @@ Option.map ContextMenu.viewMenuPopup model.contextMenu
        ]
    )
