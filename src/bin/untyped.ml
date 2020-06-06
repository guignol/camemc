
type node =
    | Node_No_Op
    | Node_Int of		int
    | Node_Binary of	Parser.operation * node * node
    | Node_Variable of	int * string * int
    | Node_Assign of	int * node * node
    | Node_Return of	node
    | Node_If of		node * node * node
    | Node_While of		node * node
    | Node_For of		node * node * node * node
    | Node_Block of		node list
    | Node_Call of		string * node list
    | Node_Address of	node
    | Node_Deref of		int * node

let un_typed offset_list node =
    let rec convert = function
        | Typed.Node_No_Op -> Node_No_Op
        | Typed.Node_Int (_, num) -> Node_Int num
        | Typed.Node_Binary (_, op, left, right) -> 
            Node_Binary (op, convert left, convert right)
        | Typed.Node_Variable (c_type, name, index) ->
            Node_Variable (Type.size c_type, name, List.nth offset_list index)
        | Typed.Node_Assign (c_type, left, right) ->
            Node_Assign (Type.size c_type, convert left, convert right)
        | Typed.Node_Return (_, node) -> Node_Return (convert node)
        | Typed.Node_If (condition, if_true, if_false) ->
            Node_If (convert condition, convert if_true, convert if_false)
        | Typed.Node_While (condition, execution) ->
            Node_While (convert condition, convert execution)
        | Typed.Node_For (init, condition, iteration, execution) ->
            Node_For (convert init, convert condition, convert iteration, convert execution)
        | Typed.Node_Block nodes -> Node_Block (List.map convert nodes)
        | Typed.Node_Call (_, name, args) -> Node_Call (name, List.map convert args)
        | Typed.Node_Address (_, node) -> Node_Address (convert node)
        | Typed.Node_Deref (c_type , node) ->
            Node_Deref (Type.size c_type, convert node)
    in
    convert node


type parameter = {
    p_name: string;
    size: int;
    offset: int
}

type global = 
    | Function of string * parameter list * node list * (* stack *) int

let rec offset_list list sum = function
    | [] -> (list, sum)
    | head :: tail ->
        let { Type.c_type; _} = head in
        let size = Type.size c_type in
        let sum = sum + size in
        let list = list @ [sum] in
        offset_list list sum tail

let untyped globals = 
    let rec t converted = function 
        | [] -> converted
        | global :: globals -> match global with
              Typed.Function (_, name, params, body, locals) ->
                let (offset_list, stack) = offset_list [] 0 locals in
                let body = List.map (un_typed offset_list) body in
                let params = List.mapi
                        (fun i p -> 
                             let name = p.Type.name in
                             let size = Type.size p.Type.c_type in
                             let offset = List.nth offset_list i in
                             { p_name = name; size; offset }
                        ) params
                in
                let f = Function (name, params, body, stack) in
                t (converted @ [f]) globals
    in
    t [] globals 