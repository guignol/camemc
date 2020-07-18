
type c_type =
    | UNDEFINED
    | INT 
    (* | TYPE_BOOL  *)
    | POINTER of c_type
    | ARRAY of int * c_type

let rec size = function
    | UNDEFINED -> 0
    | INT -> 4
    | POINTER _ -> 8
    | ARRAY (s, e) -> s * size e

let is_pointer	= function POINTER _ -> true | _ -> false
let is_array	= function ARRAY _ -> true | _ -> false

let rec same (type_1, type_2) =
    match type_1, type_2 with
    | INT, INT -> true
    | POINTER p_1, POINTER p_2 -> same (p_1, p_2)
    | ARRAY (s_1, e_1), ARRAY (s_2, e_2) ->
        if s_1 != s_2 then false else same (e_1, e_2)
    | UNDEFINED, _ -> failwith ""
    | _, UNDEFINED -> failwith ""
    | _ -> false

let rec to_string = function
    | UNDEFINED -> "UNDEFINED"
    | INT -> "INT"
    | POINTER pointed -> "POINTER of " ^ to_string pointed
    | ARRAY (_, element) -> "ARRAY of " ^ to_string element

type typed_name = {
    c_type: c_type;
    name: string;
}
