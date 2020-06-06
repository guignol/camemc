
open Type

type node =
    | Node_No_Op
    | Node_Int of		c_type * int
    | Node_Binary of	c_type * Parser.operation * node * node
    | Node_Variable of	c_type * string * int
    | Node_Assign of	c_type * node * node
    | Node_Return of	c_type * node
    | Node_If of		node * node * node
    | Node_While of		node * node
    | Node_For of		node * node * node * node
    | Node_Block of		node list
    | Node_Call of		c_type * string * node list
    | Node_Address of	c_type * node
    | Node_Deref of		c_type * node

let get_type = function
    | Node_No_Op	-> failwith "Node_No_Op has no type"
    | Node_Int		(c_type, _)			-> c_type
    | Node_Binary	(c_type, _, _, _)	-> c_type
    | Node_Variable	(c_type, _, _)		-> c_type
    | Node_Assign	(c_type, _, _)		-> c_type
    | Node_Return	(c_type, _)			-> c_type
    | Node_If		_ -> failwith "Node_If has no type"
    | Node_While	_ -> failwith "Node_While has no type"
    | Node_For		_ -> failwith "Node_For has no type"
    | Node_Block	_ -> failwith "Node_Block has no type"
    | Node_Call		(c_type, _, _)		-> c_type
    | Node_Address	(c_type, _)			-> c_type
    | Node_Deref	(c_type, _)			-> c_type

let with_type locals node =
    let rec convert = function
        | Parser.Node_No_Op -> Node_No_Op
        | Parser.Node_Int d -> Node_Int (TYPE_INT, d)
        | Parser.Node_Binary (op, left, right) -> 
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            let type_b = 
                match op with
                | Parser.PLUS | Parser.MINUS -> 
                    if is_pointer left_t then TYPE_POINTER left_t else
                    let right_t = get_type right in
                    if is_pointer right_t then TYPE_POINTER right_t else TYPE_INT
                | Parser.MUL | Parser.DIV -> left_t
                | _ -> TYPE_INT (* TYPE_BOOL *)
            in
            Node_Binary (type_b, op, left, right)
        | Parser.Node_Variable (name, index) ->
			let { c_type; _ } = List.nth locals index in
            Node_Variable (c_type, name, index)
        | Parser.Node_Assign (left, right) ->
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            (* TODO 左右の型比較はここでやる？ *)
            Node_Assign (left_t, left, right)
        | Parser.Node_Return node ->
            let node = convert node in
            let t = get_type node in
            (* TODO 関数の返り値の型と比較 *)
            Node_Return (t, node)
        | Parser.Node_If (condition, if_true, if_false) -> 
            Node_If (convert condition, convert if_true, convert if_false)
        | Parser.Node_While (condition, execution) -> 
            Node_While ((convert condition), (convert execution))
        | Parser.Node_For (init, condition, iteration, execution) -> 
            Node_For (convert init, convert condition, convert iteration, convert execution)
        | Parser.Node_Block nodes -> Node_Block (List.map convert nodes)
        | Parser.Node_Call (name, args) -> 
            let args = List.map convert args in
            (* TODO 関数の前方宣言から帰り値の型を探す? *)
            Node_Call (TYPE_INT, name, args)
        | Parser.Node_Address node ->
            let sub_node = convert node in
            let t = TYPE_POINTER (get_type sub_node) in
            Node_Address (t, sub_node)
        | Parser.Node_Deref node ->
            let sub_node = convert node in
            let t = match get_type sub_node with
                | TYPE_POINTER pointed -> pointed
                | _ -> failwith "it should be pointer type."
            in
            Node_Deref (t, sub_node)
    in
    convert node

type global = 
    | Function of c_type * string * typed_name list * node list * typed_name list

let typed globals = 
    let rec t converted = function 
        | [] -> converted
        | global :: globals -> match global with
              Parser.Function ({ name; c_type }, params, body, locals) ->
                let body = List.map (with_type locals) body in
                let f = Function (c_type, name, params, body, locals) in
                t (converted @ [f]) globals
    in
    t [] globals
