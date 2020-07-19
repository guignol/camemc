
type parameter = {
    name: string;
    size: int;
    offset: int
}

let untyped_node offset_of_index node =
    let rec convert = function
        | Node.Nop -> Node.Nop
        | Node.Int		num -> Int num
        | Node.SizeOf	node ->					SizeOf		(convert node)
        | Node.Binary	(meta, op, l, r) ->		Binary		(Type.size meta, op, convert l, convert r)
        | Node.Variable	(meta, name, i, arr) ->	Variable	(Type.size meta, name, offset_of_index i, arr)
        | Node.Global	(meta, name) ->			Global		(Type.size meta, name)
        | Node.Assign	(meta, l, r) ->			Assign		(Type.size meta, convert l, convert r)
        | Node.Return	node ->					Return		(convert node)
        | Node.If		(c, t, f) ->			If			(convert c, convert t, convert f)
        | Node.While	(c, e) ->				While		(convert c, convert e)
        | Node.For		(i, c, iter, e) ->		For			(convert i, convert c, convert iter, convert e)
        | Node.Block	nodes ->				Block		(List.map convert nodes)
        | Node.Call		(meta, name, args) ->	Call		(Type.size meta, name, List.map convert args)
        | Node.Address	node ->					Address		(convert node)
        | Node.Deref	(meta, node) ->			Deref		(Type.size meta, convert node)
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
    let rec convert converted = function 
        | [] -> converted
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
                let f = Global.Function (name, params, body, stack) in
                convert (converted @ [f]) globals
            | Global.Variable (c_type, { Type.name; _}) -> 
                let g = Global.Variable (Type.size c_type, name) in
                convert (converted @ [g]) globals
    in
    convert [] globals 