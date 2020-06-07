
type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type 'meta node =
    | Nop
    | Int of int
	| SizeOf of		'meta node
    | Binary of		'meta * operation * 'meta node * 'meta node
    | Variable of	'meta * string * int * bool (* is array *)
    | Assign of		'meta * 'meta node * 'meta node
    | Return of		'meta node
    | If of			'meta node * 'meta node * 'meta node
    | While of		'meta node * 'meta node
    | For of		'meta node * 'meta node * 'meta node * 'meta node
    | Block of		'meta node list
    | Call of		'meta * string * 'meta node list
    | Address of	'meta * 'meta node
    | Deref of		'meta * 'meta node
    | Expr_Statement of 'meta * 'meta node

let convert mm ii node =
    let rec ff = function
        | Nop -> Nop
        | Int		num -> Int num
		| SizeOf	node ->					SizeOf		(ff node)
        | Binary	(meta, op, l, r) ->		Binary		(mm meta, op, ff l, ff r)
        | Variable	(meta, name, i, arr) ->	Variable	(mm meta, name, ii i, arr)
        | Assign	(meta, l, r) ->			Assign		(mm meta, ff l, ff r)
        | Return	node ->					Return		(ff node)
        | If		(c, t, f) ->			If			(ff c, ff t, ff f)
        | While		(c, e) ->				While		(ff c, ff e)
        | For		(i, c, iter, e) ->		For		(ff i, ff c, ff iter, ff e)
        | Block		nodes ->				Block		(List.map ff nodes)
        | Call		(meta, name, args) ->	Call		(mm meta, name, List.map ff args)
        | Address	(meta, node) ->			Address	(mm meta, ff node)
        | Deref		(meta, node) ->			Deref		(mm meta, ff node)
        | Expr_Statement (meta, node) ->	Expr_Statement (mm meta, ff node)
    in
    ff node

type ('name, 'param, 'meta, 'stack) global = 
    | Function of 'name * 'param list * 'meta node list * 'stack