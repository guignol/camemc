
type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type 'meta (* null -> type -> int *) node =
    | Nop
    | Int of int
    | SizeOf of		'meta node
    | Binary of		'meta * operation * 'meta node * 'meta node
    | Variable of	'meta * string * int (* index -> offset *) * bool (* is array *)
    | Global of		'meta * string
    | Assign of		'meta * 'meta node * 'meta node
    | Return of		'meta node
    | If of			'meta node * 'meta node * 'meta node
    | While of		'meta node * 'meta node
    | For of		'meta node * 'meta node * 'meta node * 'meta node
    | Block of		'meta node list
    | Call of		'meta * string * 'meta node list
    | Address of	'meta node
    | Deref of		'meta * 'meta node
	| Indexed of	'meta * 'meta node
    | Expr_Statement of 'meta node
