
type c_type =
    | TYPE_INT 
    (* | TYPE_BOOL  *)
    | TYPE_POINTER of c_type
	| TYPE_UNDEFINED

let size = function
    | TYPE_INT -> 4
    | TYPE_POINTER _ -> 8
	| TYPE_UNDEFINED -> 0

let is_pointer = function TYPE_POINTER _ -> true | _ -> false

type typed_name = {
    c_type: c_type;
    name: string;
}
