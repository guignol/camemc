
type ('name, 'param, 'meta, 'stack) top_level = 
    | Function of 'name * 'param list * 'meta Node.node list * 'stack
	| Variable of 'meta * 'name