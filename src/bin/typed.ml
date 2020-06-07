
let rec get_type = function
    | Node.Nop		-> failwith "Nop has no type"
    | Node.Int		_					-> Type.INT
	| Node.SizeOf		_				-> Type.INT
    | Node.Binary	(c_type, _, _, _)	-> c_type
    | Node.Variable	(c_type, _, _)		-> c_type
    | Node.Assign	(c_type, _, _)		-> c_type
    | Node.Return	node				-> get_type node
    | Node.If		_ -> failwith "If has no type"
    | Node.While	_ -> failwith "While has no type"
    | Node.For		_ -> failwith "For has no type"
    | Node.Block	_ -> failwith "Block has no type"
    | Node.Call		(c_type, _, _)		-> c_type
    | Node.Address	(c_type, _)			-> c_type
    | Node.Deref	(c_type, _)			-> c_type
    | Node.Expr_Statement _ -> failwith "Expr_Statement has no type"

let with_type locals node =
    let rec convert = function
        | Node.Nop -> Node.Nop
        | Node.Int d -> Node.Int d
		| Node.SizeOf	node -> Node.Int (Type.size (get_type (convert node)))
        | Node.Binary (_, op, left, right) -> 
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            let right_t = get_type right in
            begin
                match op with
                | Node.PLUS | Node.MINUS ->
                    begin
                        match (left_t, right_t) with
                        | (Type.INT, Type.INT) -> 
                            Node.Binary (Type.INT, op, left, right)
                        | (Type.INT, Type.POINTER pointed) ->
                            let weight = Type.size pointed in
                            let left = Node.Binary (Type.INT, Node.MUL, left, Node.Int weight) in
                            Binary (Type.POINTER pointed, op, left, right)
                        | (Type.POINTER pointed, Type.INT) -> 
                            let weight = Type.size pointed in
                            let right = Node.Binary (Type.INT, Node.MUL, right, Node.Int weight) in
                            Node.Binary (Type.POINTER pointed, op, left, right)
                        | (Type.POINTER pointed, Type.POINTER _) as pp ->
                            if op = PLUS (* ポインタ同士の足し算はできない *)
                            then failwith "cannot add with pointers" else
                            if not (Type.same pp) (* 型の異なるポインタ同士の引き算はできない *)
                            then failwith "cannot subtract with different pointers" else 
                            let substarcted = Node.Binary (Type.INT, op, left, right) in
                            (* ポイントされているサイズで割る *)
                            let weight = Type.size pointed in
                            Node.Binary (Type.INT, Node.DIV, substarcted, Node.Int weight)
                        | _ -> failwith "cannot add/subtract"
                    end
                | Node.MUL | Node.DIV -> 
                    begin
                        match (left_t, right_t) with (* 掛け算と割り算は数値のみ *)
                        | (Type.INT, Type.INT) -> Node.Binary (left_t, op, left, right)
                        | _ -> failwith "cannot multiply/divide" 
                    end
                | _ -> (* TODO Type.BOOL *)
                    Node.Binary (left_t, op, left, right)
            end
        | Node.Variable (_, name, index) ->
            let { Type.c_type; _ } = List.nth locals index in
            Node.Variable (c_type, name, index)
        | Node.Assign (_, left, right) ->
            let left = convert left in
            let right = convert right in
            let left_t = get_type left in
            (* TODO 左右の型比較はここでやる？ *)
            Node.Assign (left_t, left, right)
        | Node.Return node ->
            let node = convert node in
            let _ = get_type node in
            (* TODO 関数の返り値の型と比較 *)
            Node.Return node
        | Node.If (c, t, f) -> Node.If (convert c, convert t, convert f)
        | Node.While (c, loop) -> Node.While (convert c, convert loop)
        | Node.For (i, c, iter, loop) -> Node.For (convert i, convert c, convert iter, convert loop)
        | Node.Block nodes -> Node.Block (List.map convert nodes)
        | Node.Call (_, name, args) -> 
            let args = List.map convert args in
            (* TODO 関数の前方宣言から帰り値の型を探す? *)
            Node.Call (Type.INT, name, args)
        | Node.Address (_, node) ->
            let pointed = convert node in
            Node.Address (Type.POINTER (get_type pointed), pointed)
        | Node.Deref (_, node) ->
            let target = convert node in
            let t = match get_type target with
                | Type.POINTER pointed -> pointed
                | _ -> failwith "it should be pointer type."
            in
            Node.Deref (t, target)
        | Node.Expr_Statement (_, node) -> Node.Expr_Statement (Type.UNDEFINED, convert node)
    in
    convert node

let typed globals = 
    let rec t converted = function 
        | [] -> converted
        | global :: globals -> match global with
              Node.Function (returned, params, body, locals) ->
                let body = List.map (with_type locals) body in
                let f = Node.Function (returned, params, body, locals) in
                t (converted @ [f]) globals
    in
    t [] globals
