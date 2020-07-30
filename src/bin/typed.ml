
let rec get_type = function
    | Node.Nop		-> failwith "Nop has no type"
    | Node.Int		_					-> Type.INT
    | Node.String		_				-> Type.POINTER Type.CHAR
    | Node.SizeOf		_				-> Type.INT
    | Node.Binary	(c_type, _, _, _)	-> c_type
    | Node.Variable	(c_type, _, _, _)	-> c_type
    | Node.Global	(c_type, _)			-> c_type
    | Node.Assign	(c_type, _, _)		-> c_type
    | Node.Return	node				-> get_type node
    | Node.If		_ -> failwith "If has no type"
    | Node.While	_ -> failwith "While has no type"
    | Node.For		_ -> failwith "For has no type"
    | Node.Block	_ -> failwith "Block has no type"
    | Node.Call		(c_type, _, _)		-> c_type
    | Node.Address	node				-> Type.POINTER (get_type node)
    | Node.Deref	(c_type, _)			-> c_type
    | Node.Indexed	(c_type, _)			-> c_type
    | Node.Expr_Statement _ -> failwith "Expr_Statement has no type"

let is_degit	= function Type.INT | Type.CHAR -> true | _ -> false

let with_type locals globals node =
    let rec convert = function
        | Node.Nop -> Node.Nop
        | Node.Int d -> Node.Int d
        | Node.String label -> Node.String label
        | Node.SizeOf node -> Node.Int (Type.size (get_type (convert node)))
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
                        | (d1, d2) when is_degit d1 && is_degit d2 ->
                            Node.Binary (Type.INT, op, left, right)
                        | (d1, Type.POINTER pointed)
                        | (d1, Type.ARRAY (_, pointed)) when is_degit d1 -> 
                            let weight = Type.size pointed in
                            let left = Node.Binary (Type.INT, Node.MUL, left, Node.Int weight) in
                            Binary (Type.POINTER pointed, op, left, right)
                        | (Type.POINTER pointed, d2) 
                        | (Type.ARRAY (_, pointed), d2) when is_degit d2 -> 
                            let weight = Type.size pointed in
                            let right = Node.Binary (Type.INT, Node.MUL, right, Node.Int weight) in
                            Node.Binary (Type.POINTER pointed, op, left, right)
                        | (Type.POINTER pointed, Type.POINTER _) as pp ->
                            (* TODO 配列も混ぜて引き算できる？ *)
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
                | Node.MUL | Node.DIV -> (* 掛け算と割り算は数値のみ *)
                    if is_degit left_t && is_degit right_t
                    then Node.Binary (Type.INT, op, left, right)
                    else failwith "cannot multiply/divide"
                | _ -> (* TODO Type.BOOL *)
                    Node.Binary (left_t, op, left, right)
            end
        | Node.Variable (_, name, index, array) ->
            let { Type.c_type; _ } = List.nth locals index in
            Node.Variable (c_type, name, index, array)
        | Node.Global (_, name) ->
            (* グローバル変数 *)
            let by_name g = g.Type.name = name in
            let { Type.c_type; Type.name } = List.find by_name globals in 
            Node.Global (c_type, name)
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
        | Node.Address node ->
            let pointed = convert node in
            Node.Address pointed
        | Node.Deref (_, node) ->
            let target = convert node in
            begin match get_type target with
            | Type.POINTER pointed -> Node.Deref (pointed, target)
            | Type.ARRAY (_, element) -> Node.Deref (element, target)
            | _ -> failwith "it should be pointer type or array."
            end
        | Node.Indexed (_, node) ->
            let target = convert node in
            begin match get_type target with
            | Type.POINTER element
            | Type.ARRAY (_, element) -> 
                if Type.is_array element
                (* 配列から配列を取り出す場合 *)
                then Node.Indexed (element, target)
                (* 配列から配列以外の要素を取り出す場合 *)
                else Node.Deref (element, target)
            | _ -> failwith "it should be pointer type or array."
            end
        | Node.Expr_Statement node -> Node.Expr_Statement (convert node)
    in
    convert node

let typed top_levels = 
    let rec t converted globals = function 
        | [] -> converted
        | head :: top_levels -> match head with
            | Global.Function (returned, params, body, locals) ->
                let body = List.map (with_type locals globals) body in
                let f = Global.Function (returned, params, body, locals) in
                t (converted @ [f]) globals top_levels
            | Global.Variable (_, name) ->
                let g = Global.Variable (name.Type.c_type, name) in
                t (converted @ [g]) (name :: globals) top_levels
            | Global.String (label, literal) -> 
                let s = Global.String (label, literal) in
                t (converted @ [s]) globals top_levels
    in
    t [] [] top_levels
