
type 'a menuItem  = string * 'a
type 'a menu = 'a menuItem list


open Tea.Html


open Tea.Json

type globalMenuState = {
    mutable offsets: int * int;
}
let globalMenuState = {offsets=(0, 0)}


let viewMenuItem item =
    let label, cb = item in
    Js.log cb;
    button [onCB "click" "" (fun _ -> Some cb)] [text label]

let viewMenuPopup menu =
    let x, y = globalMenuState.offsets in
    div [ class' "context-menu-popup"
        ; style "left" (String.concat "" [string_of_int x; "px"])
        ; style "top" (String.concat "" [string_of_int y; "px"]);
        ] @@
        List.map viewMenuItem menu

let contextMenuContainer createMsg ?key:((key: string)="") node props children: ('a Vdom.t) =
    let eventHandler = onCB "contextmenu" key (fun event ->
        let mapper = Decoder.map2 (fun x y ->
            globalMenuState.offsets <- (x, y);
        ) (Decoder.field "clientX" Decoder.int) (Decoder.field "clientY" Decoder.int) in
        let error = Decoder.decodeEvent mapper event
            |> Tea.Result.error
        in match error with
        | Some e -> Js.log e; None
        | None -> Js.log createMsg; Some createMsg
    ) in
    node (eventHandler :: props) children

