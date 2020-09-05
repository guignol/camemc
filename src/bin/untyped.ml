
type parameter = {
    name: string;
    size: int;
    offset: int
}

let untyped_node offset_of_name_scoped node =
    let rec convert = function
        | Node.Nop -> Node.Nop
        | Node.Int		num -> Int num
        | Node.String	label -> String label
        | Node.SizeOf	node ->					SizeOf		(convert node)
        | Node.Binary	(c_type, op, l, r) ->	Binary		(Type.size c_type, op, convert l, convert r)
        | Node.Variable	(c_type, name, s, a) ->	Variable	(Type.size c_type, name, offset_of_name_scoped name s, a)
        | Node.Global	(c_type, name) ->		Global		(Type.size c_type, name)
        | Node.Assign	(c_type, l, r) ->		Assign		(Type.size c_type, convert l, convert r)
        | Node.Return	node ->					Return		(convert node)
        | Node.If		(c, t, f) ->			If			(convert c, convert t, convert f)
        | Node.While	(c, e) ->				While		(convert c, convert e)
        | Node.For		(i, c, iter, e) ->		For			(convert i, convert c, convert iter, convert e)
        | Node.Block	nodes ->				Block		(List.map convert nodes)
        | Node.Call		(c_type, name, args) ->	Call		(Type.size c_type, name, List.map convert args)
        | Node.Address	node ->					Address		(convert node)
        | Node.Deref	(c_type, node) ->		Deref		(Type.size c_type, convert node)
        | Node.Indexed	(_, node) ->			Indexed		(0, convert node)
        | Node.Expr_Statement node ->			Expr_Statement (convert node)
    in
    convert node

let rec calculate_offsets list offset = function
    | [] -> list, offset
    | head :: tail ->
        let { Type.c_type; Type.name}, scope = head in
        let offset = offset + Type.size c_type in
        let head = { Type.c_type; Type.name}, scope, offset in
        calculate_offsets (head :: list) offset tail

let untyped globals = 
    let rec convert = function 
        | [] -> []
        | global :: globals -> match global with
            | Global.Function ({ Type.name; _}, params, body, locals) ->
                let locals, stack = calculate_offsets [] 0 locals in
                let offset_of_name_scoped n s = 
                    let offset = List.find_map
                            (fun ({ Type.name; _}, scope, offset) ->
                                 if n = name && s = scope
                                 then Some offset
                                 else None
                            ) locals
                    in
					match offset with 
					| Some offset -> offset
					| None -> failwith ("variable " ^ n ^ " is not found")
                in
                let body = List.map (untyped_node offset_of_name_scoped) body in
                let params = List.map
                        (fun { Type.name; Type.c_type; } -> 
                             let size = Type.size c_type in
                             let offset = offset_of_name_scoped name 0 in
                             { name; size; offset }
                        ) params
                in
                Global.Function (name, params, body, stack) :: convert globals
            | Global.Variable (c_type, { Type.name; _}) -> 
                Global.Variable (Type.size c_type, name) :: convert globals
            | Global.String (label, literal) -> 
                Global.String (label, literal) :: convert globals
    in
    convert globals 

(* 
	そのローカルスコープでの名前の重複を防いで、名前と型のペアを保存する
	この時点ではオフセットが決まらなくてよい
	横並びのスコープに対してスタックを節約しなくてよい
	変数の登録とスコープの管理を分ける？
 *)