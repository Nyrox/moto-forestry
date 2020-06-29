

module Path = struct
    type t = {repr: string}

    let absolute path =
        {repr=path}

    let resolve path =
        absolute @@ Node.Path.resolve path "."

    let relative a b =
        Node.Path.relative ~from:a.repr ~to_:b.repr ()

    let extname: (t -> string) = [%raw {|
    function(p) {
        return require("path").extname(p.repr)
    }
    |}]

    let basename file =
        Node.Path.basename file.repr

    let dirname file =
        Node.Path.dirname file.repr

    let asString file =
        file.repr

    let extend path segment =
        absolute @@ Node.Path.resolve path.repr segment
end

let loadFile: (string -> string Js.Promise.t) = [%raw {|
function (path) {
    let __impl = require("util").promisify(require("fs").readFile)
    return __impl(path, "utf-8")
}
|}]

let loadFileSync: (string -> string) = [%raw {|
function (path) {
    return require("fs").readFileSync(path, "utf-8")
}
|}]

let writeFile: (string -> string -> unit) = [%raw {|
function (path, content) {
    require("fs").writeFile(path, content, _ => undefined)
}
|}]

external __static: string = "__static" [@@bs.val]

let getStaticAssetPath name =
    name
    