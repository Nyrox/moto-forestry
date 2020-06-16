
type 'a t = 'a option

let map fn v = match v with
| Some i -> Some (fn i)
| None -> None

let and_then fn v = match v with
| Some i -> fn i
| None -> None

let orDefault def o = match o with
| Some i -> i
| None -> def

let isSome o = match o with 
| Some _ -> true
| None -> false

let isNone o = not @@ isSome o