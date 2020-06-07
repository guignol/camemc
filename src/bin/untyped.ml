
type parameter = {
    name: string;
    size: int;
    offset: int
}

let size_of_type m = Type.size m

let rec offset_list list sum = function
    | [] -> (list, sum)
    | head :: tail ->
        let { Type.c_type; _} = head in
        let size = size_of_type c_type in
        let sum = sum + size in
        let list = list @ [sum] in
        offset_list list sum tail

let untyped globals = 
    let rec t converted = function 
        | [] -> converted
        | global :: globals -> match global with
              Node.Function ({ Type.name; _}, params, body, locals) ->
                let (offset_list, stack) = offset_list [] 0 locals in
                let offset_of_index i = List.nth offset_list i in
                let un_typed node = Node.convert size_of_type offset_of_index node in
                let body = List.map un_typed body in
                let params = List.mapi
                        (fun i { Type.name; Type.c_type; } -> 
                             let size = size_of_type c_type in
                             let offset = offset_of_index i in
                             { name; size; offset }
                        ) params
                in
                let f = Node.Function (name, params, body, stack) in
                t (converted @ [f]) globals
    in
    t [] globals 