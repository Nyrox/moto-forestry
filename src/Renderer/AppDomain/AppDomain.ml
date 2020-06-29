open Utils

type vertexInputData = 
| Mesh of Path.t
| FullScreenPass
| NoData
[@@bs.deriving accessors]





module Pass = struct
    type t = {
        name: string;
        vertexInput: vertexInputData;
        fragShader: Path.t option;
    }

    let createEmpty name =
        {name; vertexInput=NoData; fragShader=None}
end
type pass = Pass.t

(* Project *)
module Project = struct
    type t = {
        passes: pass list;
    }

    let createPass project passName =
        {project with passes=(Pass.createEmpty passName) :: project.passes}

    let updatePass (project: t) name pass =
        {project with passes=(List.map (fun (p: pass) -> if p.name == name then pass else p) project.passes)}
end
type project = Project.t

(* Create a new empty project *)
let createProject (): Project.t =
    {
        passes=[]
    }


(* Json Decode *)
module Decode = struct
    open Json.Decode

    let vertexInput json: vertexInputData =
        (field "type" string |> andThen (
            function    | "mesh" -> Json.Decode.map (fun s -> mesh @@ Path.absolute s) @@ field "path" string
                        | "fullscreen" -> (fun _ -> FullScreenPass)
                        | "nodata" -> (fun _ -> NoData)
                        | _ -> failwith "Unexpected vertexInput variant"
        )) json

    let pass json: Pass.t =
        {
            name = json |> field "name" string;
            vertexInput = json |> field "vertexInput" vertexInput;
            fragShader = Option.map Path.absolute (json |> optional @@ field "fragShader" string);
        }

    
    let project json: Project.t =
        {
            passes = json |> field "passes" (list pass)
        }
end


(* Json Encode *)
module Encode = struct
    open Json.Encode

    let vertexInput v =
        (
            object_ @@
                (v |> function
                | Mesh p -> ["type", string "mesh"; "path", string @@ Path.asString p]
                | FullScreenPass -> ["type", string "fullscreen"]
                | NoData -> ["type", string "nodata"])
        )

    let pass (p: Pass.t) = 
        (
            object_ [
                ("name", string p.name);
                ("vertexInput", vertexInput p.vertexInput)
            ]
        )

    let project (p: Project.t) =
        (
            object_ [
                ("passes", list pass p.passes)
            ]
        )
end