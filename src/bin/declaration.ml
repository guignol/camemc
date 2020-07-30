
type scope = Locally | Globally
(* ローカル変数宣言 *)
let locals: Type.typed_name list ref = ref []
(* グローバル変数宣言 *)
let globals: Type.typed_name list ref = ref []

let find scope name =
    let rec find index = function
        | [] -> None
        | head :: tail -> 
            if head.Type.name = name 
            then Some (head, index)
            else find (index + 1) tail
    in
    match scope with 
    | Locally -> find 0 !locals
    | Globally -> find 0 !globals

let add scope declaration =
	let { Type.name; _ } = declaration in
    let scope_name, scope_ref = match scope with Locally -> "local", locals | Globally -> "global", globals in
    match find scope name with
    | Some _ -> failwith (scope_name ^ " variable " ^ name ^ " is already declared.")
    | None ->
        scope_ref := !scope_ref @ [declaration];
        ()