(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf
open String

(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)

type operation = Plus | Minus

type token =
    | Number of int
    | Operator of operation
    | Identifier of string

let starts_with str ch = match index_opt str ch with
    | Some index -> index = 0
    | None -> false

let numbers = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']

let rec starts_with_any_of_list str = function
    | [] -> false
    | head :: tail -> starts_with str head || starts_with_any_of_list str tail

let starts_with_number str = starts_with_any_of_list str numbers

let tokenize = function
        | "+" -> Operator Plus
        | "-" -> Operator Minus
        | input when starts_with_number input -> Number (int_of_string input)
        | _ as input -> Identifier input

let print_token = function
    Number d -> printf "# int: %d\n" d
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
    | Node_Int of int
    | Node_Calc of operation * node * node

let expect_int = function
    (Number d) :: tokens -> (Node_Int d, tokens)
    | _ -> failwith "this is not int"

(*
expr = num ("+" num | "-" num)*
*)

let rec add left = function
        | [] -> (left, [])
        | head :: tail -> match head with
            | Operator op ->
                let (right, tail) = expect_int tail in
                let node = Node_Calc (op, left, right) in
                add node tail
            | _ -> (left, head :: tail)

let parse tokens =
    let (left, tokens) = expect_int tokens in
    let (node, _) = add left tokens in
    node

let ast = parse (read 1)

let rec p = function
    | Node_Int d -> printf " %d" d
    | Node_Calc (op, left, right) ->
        p left;
        begin match op with
        | Plus -> printf " +"
        | Minus -> printf " -"
        end;
        p right

let rec emit = function
    | Node_Int d -> printf "  push %d\n" d
    | Node_Calc (op, left, right) ->
        emit left;
        emit right;
        print_string   "  pop rdi\n";
        print_string   "  pop rax\n";
        begin match op with
        | Plus -> print_string   "  add rax, rdi\n";
        | Minus -> print_string   "  sub rax, rdi\n";
        end;
        print_string   "  push rax\n"

let () =
    printf "#";
    p ast;
    print_endline "";
    print_endline  ".intel_syntax noprefix";
    print_endline  ".text";
    print_endline  ".global main";
    print_endline  "main:";
    (* プロローグ *)
    print_endline  "  push rbp";
    print_endline  "  mov rbp, rsp";
    emit ast;
    print_endline  ".Lreturn.main:";
    (* エピローグ *)
    print_endline  "  mov rsp, rbp";
    print_endline  "  pop rbp";
    print_endline  "  ret";