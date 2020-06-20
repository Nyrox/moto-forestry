

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

external __static: string = "__static" [@@bs.val]

let getStaticAssetPath name =
    name
    