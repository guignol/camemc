(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf

type operation = Plus | Minus

type token =
    Degit of int
    | Operator of operation
    | Identifier of string

let tokenize input =
    match input with
        ("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0") -> Degit (int_of_string input)
        |"+" -> Operator Plus
        |"-" -> Operator Minus
        | _ -> Identifier input

let print_token = function
    Degit d -> printf "# int: %i\n" d
    | Operator op ->
        (match op with
            Plus -> printf "# operator: %s\n" "+"
            | Minus -> printf "# operator: %s\n" "-")
    | Identifier s -> printf "# identifier: %s\n" s

let count = Array.length Sys.argv - 1

let rec read i =
    if i = count then []
    else tokenize Sys.argv.(i) :: read (i + 1)

let () = List.iter print_token (read 1)

let () = print_endline  ".intel_syntax noprefix"
let () = print_endline  ".text"
let () = print_endline  ".global main"
let () = print_endline  "main:"
let () = print_endline  "  push rbp"
let () = print_endline  "  mov rbp, rsp"
let () = printf         "  mov rax, %i\n" count
let () = print_endline  ".Lreturn.main:"
let () = print_endline  "  mov rsp, rbp"
let () = print_endline  "  pop rbp"
let () = print_endline  "  ret"