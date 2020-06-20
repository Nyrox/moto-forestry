open Utils

type vertexInputData = 
| Mesh of Path.t
| NoData
[@@bs.deriving accessors]





module Pass = struct
    type t = {
        name: string;
        vertexInput: vertexInputData;
    }

    let createEmpty name =
        {name; vertexInput=NoData}
end
type pass = Pass.t

(* Project *)
module Project = struct
    type t = {
        passes: pass list;
    }

    let createPass project passName =
        {project with passes=(Pass.createEmpty passName) :: project.passes}
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
                        | "nodata" -> (fun _ -> NoData)
                        | _ -> failwith "Unexpected vertexInput variant"
        )) json

    let pass json: Pass.t =
        {
            name = json |> field "name" string;
            vertexInput = json |> field "vertexInput" vertexInput
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
                | Mesh p -> ["type", string "mesh"; "mesh", string @@ Path.asString p]
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