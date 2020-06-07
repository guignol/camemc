
type c_type =
    | UNDEFINED
    | INT 
    (* | TYPE_BOOL  *)
    | POINTER of c_type

let size = function
    | UNDEFINED -> 0
    | INT -> 4
    | POINTER _ -> 8

let is_pointer = function POINTER _ -> true | _ -> false

let rec same (type_1, type_2) =
    match type_1, type_2 with
    | INT, INT -> true
    | POINTER p_1, POINTER p_2 -> same (p_1, p_2)
    | UNDEFINED, _ -> failwith ""
    | _, UNDEFINED -> failwith ""
    | _ -> false


type typed_name = {
    c_type: c_type;
    name: string;
}
