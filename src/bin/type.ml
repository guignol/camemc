
type c_type =
    | TYPE_UNDEFINED
    | TYPE_INT 
    (* | TYPE_BOOL  *)
    | TYPE_POINTER of c_type

let size = function
    | TYPE_UNDEFINED -> 0
    | TYPE_INT -> 4
    | TYPE_POINTER _ -> 8

let is_pointer = function TYPE_POINTER _ -> true | _ -> false

let rec same (type_1, type_2) =
    match type_1, type_2 with
    | TYPE_INT, TYPE_INT -> true
    | TYPE_POINTER p_1, TYPE_POINTER p_2 -> same (p_1, p_2)
    | TYPE_UNDEFINED, _ -> failwith ""
    | _, TYPE_UNDEFINED -> failwith ""
    | _ -> false


type typed_name = {
    c_type: c_type;
    name: string;
}
