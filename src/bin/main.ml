(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf

type operation = Plus | Minus

type token =
    Degit of int
    | Operator of operation
    | Identifier of string

let tokenize = function
        ("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0") as input -> Degit (int_of_string input)
        |"+" -> Operator Plus
        |"-" -> Operator Minus
        | _ as input -> Identifier input

let print_token = function
    Degit d -> printf "# int: %i\n" d
    | Operator op ->
        (match op with
            Plus -> printf "# operator: %s\n" "+"
            | Minus -> printf "# operator: %s\n" "-")
    | Identifier s -> printf "# identifier: %s\n" s

let count = Array.length Sys.argv - 1

let rec read i =
    if count < i then []
    else tokenize Sys.argv.(i) :: read (i + 1)

(*let () = List.iter print_token (read 1)*)

(***********************************************************)

type node =
    Node_Int of int
    | Node_Add of node * node
    | Node_Sub of node * node

let num = function
    (Degit d) :: tokens -> (Node_Int d, tokens)
    | _ -> failwith "this is not int"

(*
expr = num ("+" num | "-" num)*
*)

let rec add left tokens =
    let read_op = function
        Operator op -> Some op
        | _ -> None in
    let node_add left right = function
        Plus -> Node_Add (left, right)
        | Minus -> Node_Sub (left, right) in
    let maybe_add left = function
        [] -> None
        | head :: tokens -> let op = read_op head in match op with
            Some op ->
                let (right, tokens) = num tokens in
                let node = node_add left right op in
                Some (node, tokens)
            | None -> None in
    let maybe = maybe_add left tokens in
    match maybe with
    Some (node, tokens) -> add node tokens
    | None -> (left, tokens)

let parse tokens =
    let (left, tokens) = num tokens in
    let (node, _) = add left tokens in
    node

let ast = parse (read 1)

let rec p = function
    | Node_Int d ->
        printf " %d" d
    | Node_Add (left, right) ->
        p left;
        printf " +";
        p right
    | Node_Sub (left, right) ->
        p left;
        printf " -";
        p right

let () =
    printf "#";
    p ast;
    print_endline "";
    print_endline  ".intel_syntax noprefix";
    print_endline  ".text";
    print_endline  ".global main";
    print_endline  "main:";
    print_endline  "  push rbp";
    print_endline  "  mov rbp, rsp";
    printf         "  mov rax, %i\n" count;
    print_endline  ".Lreturn.main:";
    print_endline  "  mov rsp, rbp";
    print_endline  "  pop rbp";
    print_endline  "  ret";