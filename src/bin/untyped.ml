
type parameter = {
    name: string;
    size: int;
    offset: int
}

let untyped_node offset_of_index node =
    let rec convert = function
        | Node.Nop -> Node.Nop
        | Node.Int		num -> Int num
        | Node.String	label -> String label
        | Node.SizeOf	node ->					SizeOf		(convert node)
        | Node.Binary	(c_type, op, l, r) ->	Binary		(Type.size c_type, op, convert l, convert r)
        | Node.Variable	(c_type, name, i, a) ->	Variable	(Type.size c_type, name, offset_of_index i, a)
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

let rec offset_list list sum = function
    | [] -> (list, sum)
    | head :: tail ->
        let { Type.c_type; _} = head in
        let size = Type.size c_type in
        let sum = sum + size in
        let list = list @ [sum] in
        offset_list list sum tail

let untyped globals = 
    let rec convert = function 
        | [] -> []
        | global :: globals -> match global with
            | Global.Function ({ Type.name; _}, params, body, locals) ->
                let (offset_list, stack) = offset_list [] 0 locals in
                let offset_of_index i = List.nth offset_list i in
                let body = List.map (untyped_node offset_of_index) body in
                let params = List.mapi
                        (fun i { Type.name; Type.c_type; } -> 
                             let size = Type.size c_type in
                             let offset = offset_of_index i in
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