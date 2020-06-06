
open Type
open Node

let rec get_type = function
    | Node_No_Op	-> failwith "Node_No_Op has no type"
    | Node_Int		_					-> TYPE_INT
    | Node_Binary	(c_type, _, _, _)	-> c_type
    | Node_Variable	(c_type, _, _)		-> c_type
    | Node_Assign	(c_type, _, _)		-> c_type
    | Node_Return	node -> get_type node
    | Node_If		_ -> failwith "Node_If has no type"
    | Node_While	_ -> failwith "Node_While has no type"
    | Node_For		_ -> failwith "Node_For has no type"
    | Node_Block	_ -> failwith "Node_Block has no type"
    | Node_Call		(c_type, _, _)		-> c_type
    | Node_Address	(c_type, _)			-> c_type
    | Node_Deref	(c_type, _)			-> c_type
    | Node_Expr_Statement _ -> failwith "Node_Expr_Statement has no type"

let with_type locals node =
    let rec convert = function
        | Node_No_Op -> Node_No_Op
        | Node_Int d -> Node_Int d
        | Node_Binary (_, op, left, right) -> 
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            let right_t = get_type right in
            begin
                match op with
                | PLUS | MINUS ->
                    begin
                        match (left_t, right_t) with
                        | (TYPE_INT, TYPE_INT) -> 
                            Node_Binary (TYPE_INT, op, left, right)
                        | (TYPE_INT, TYPE_POINTER pointed) ->
							let weight = Type.size pointed in
							let left = Node_Binary (TYPE_INT, Node.MUL, left, Node_Int weight) in
                            Node_Binary (TYPE_POINTER pointed, op, left, right)
                        | (TYPE_POINTER pointed, TYPE_INT) -> 
							let weight = Type.size pointed in
							let right = Node_Binary (TYPE_INT, Node.MUL, right, Node_Int weight) in
                            Node_Binary (TYPE_POINTER pointed, op, left, right)
                        | (TYPE_POINTER pointed, TYPE_POINTER _) ->
                            (* TODO ポインタ同士の足し算はできない *)
                            (* TODO 型の異なるポインタ同士の引き算はできない *)
							(* TODO ポイントされているサイズで割る *)
                            let substarcted = Node_Binary (TYPE_INT, op, left, right) in
							let weight = Type.size pointed in
							Node_Binary (TYPE_INT, Node.DIV, substarcted, Node_Int weight)
                        | _ -> failwith "cannot do add/subtract"
                    end
                | MUL | DIV -> 
                    (* TODO ポインタの掛け算はできない *)
                    Node_Binary (left_t, op, left, right)
                | _ -> (* TODO TYPE_BOOL *)
                    Node_Binary (left_t, op, left, right)
            end
        | Node_Variable (_, name, index) ->
            let { c_type; _ } = List.nth locals index in
            Node_Variable (c_type, name, index)
        | Node_Assign (_, left, right) ->
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            (* TODO 左右の型比較はここでやる？ *)
            Node_Assign (left_t, left, right)
        | Node_Return node ->
            let node = convert node in
            let _ = get_type node in
            (* TODO 関数の返り値の型と比較 *)
            Node_Return node
        | Node_If (c, t, f) -> Node_If (convert c, convert t, convert f)
        | Node_While (c, loop) -> Node_While (convert c, convert loop)
        | Node_For (i, c, iter, loop) -> Node_For (convert i, convert c, convert iter, convert loop)
        | Node_Block nodes -> Node_Block (List.map convert nodes)
        | Node_Call (_, name, args) -> 
            let args = List.map convert args in
            (* TODO 関数の前方宣言から帰り値の型を探す? *)
            Node_Call (TYPE_INT, name, args)
        | Node_Address (_, node) ->
            let pointed = convert node in
            Node_Address (TYPE_POINTER (get_type pointed), pointed)
        | Node_Deref (_, node) ->
            let target = convert node in
            let t = match get_type target with
                | TYPE_POINTER pointed -> pointed
                | _ -> failwith "it should be pointer type."
            in
            Node_Deref (t, target)
		| Node_Expr_Statement (_, node) -> Node_Expr_Statement (TYPE_UNDEFINED, convert node)
    in
    convert node

let typed globals = 
    let rec t converted = function 
        | [] -> converted
        | global :: globals -> match global with
              Function (returned, params, body, locals) ->
                let body = List.map (with_type locals) body in
                let f = Function (returned, params, body, locals) in
                t (converted @ [f]) globals
    in
    t [] globals
