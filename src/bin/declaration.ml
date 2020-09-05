
(* ローカル変数宣言 *)
let locals: (Type.typed_name * int) list  ref = ref []
(* グローバル変数宣言 *)
let globals: Type.typed_name list ref = ref []

let find_globally name =
    let rec find = function
        | [] -> None
        | head :: tail -> 
            if head.Type.name = name 
            then Some head
            else find tail
    in
    find !globals

let add_globally declaration =
    let { Type.name; _ } = declaration in
    match find_globally name with
    | Some _ -> failwith ("global variable " ^ name ^ " is already declared.")
    | None ->
        globals := !globals @ [declaration];
        ()

type local_scope = 
    | Scope of local_scope (* parent *) * Type.typed_name list (* variables *) * int (* local id *)
    | NoScope

let ___local_scope_count___ = ref 0
let current_scope = ref NoScope

let new_scope parent =
    let id = !___local_scope_count___ in
    ___local_scope_count___ := id + 1;
	Scope (parent, [], id)

let prepare_parameters () = 
    locals := [];
	(* 引数のスコープは0固定 *)
    ___local_scope_count___ := 0;
    current_scope := new_scope NoScope;
    ()

let prepare_child_scope () = 
    (* 現在のスコープを親にして新しいスコープを作る *)
	let parent = !current_scope in
    current_scope := new_scope parent;
	parent

let restore_scope scope = 
	current_scope := scope;
	()

let find_locally ?(scope = !current_scope) name =
    let rec find_by_scope = function
        | NoScope -> None
        | Scope (parent, variables, id) ->
            let rec find_by_name = function
                | [] -> find_by_scope parent
                | head :: tail -> 
                    if head.Type.name = name 
                    then Some (head, id)
                    else find_by_name tail
            in
            find_by_name variables
    in
    find_by_scope scope

let add_locally declaration =
    let { Type.name; _ } = declaration in
    match !current_scope with
    | NoScope -> failwith "no scope to add to"
    | Scope (parent, variables, id) ->  
        (* 同じスコープ内で同名の変数は宣言できない *)
        let this_scope_only = Scope (NoScope, variables, id) in
        match find_locally name ~scope: this_scope_only with
        | Some _ -> failwith ("local variable " ^ name ^ " is already declared.")
        | None -> 
            locals := (declaration, id) :: !locals;
            current_scope := Scope (parent, declaration :: variables, id);
            ()
