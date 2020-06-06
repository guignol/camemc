
type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type 'meta node =
    | Node_No_Op
    | Node_Int of int
    | Node_Binary of	'meta * operation * 'meta node * 'meta node
    | Node_Variable of	'meta * string * int
    | Node_Assign of	'meta * 'meta node * 'meta node
    | Node_Return of	'meta node
    | Node_If of		'meta node * 'meta node * 'meta node
    | Node_While of		'meta node * 'meta node
    | Node_For of		'meta node * 'meta node * 'meta node * 'meta node
    | Node_Block of		'meta node list
    | Node_Call of		'meta * string * 'meta node list
    | Node_Address of	'meta * 'meta node
    | Node_Deref of		'meta * 'meta node

let convert mm ii node =
    let rec ff = function
        | Node_No_Op -> Node_No_Op
        | Node_Int		num -> Node_Int num
        | Node_Binary	(meta, op, l, r) ->		Node_Binary		(mm meta, op, ff l, ff r)
        | Node_Variable	(meta, name, index) ->	Node_Variable	(mm meta, name, ii index)
        | Node_Assign	(meta, l, r) ->			Node_Assign		(mm meta, ff l, ff r)
        | Node_Return	node ->					Node_Return		(ff node)
        | Node_If		(c, t, f) ->			Node_If			(ff c, ff t, ff f)
        | Node_While	(c, e) ->				Node_While		(ff c, ff e)
        | Node_For		(i, c, iter, e) ->		Node_For		(ff i, ff c, ff iter, ff e)
        | Node_Block	nodes ->				Node_Block		(List.map ff nodes)
        | Node_Call		(meta, name, args) ->	Node_Call		(mm meta, name, List.map ff args)
        | Node_Address	(meta, node) ->			Node_Address	(mm meta, ff node)
        | Node_Deref	(meta , node) ->		Node_Deref		(mm meta, ff node)
    in
    ff node

type ('name, 'param, 'meta, 'stack) global = 
    | Function of 'name * 'param list * 'meta node list * 'stack