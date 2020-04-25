(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf

let () =
  for i = 0 to Array.length Sys.argv - 1 do
    printf "# [%i] %s\n" i Sys.argv.(i)
  done;;

let () = print_endline ".intel_syntax noprefix"
let () = print_endline ".text"
let () = print_endline ".global main"
let () = print_endline "main:"
let () = print_endline "  push rbp"
let () = print_endline "  mov rbp, rsp"
let () = print_endline "  mov rax, 3"
let () = print_endline ".Lreturn.main:"
let () = print_endline "  mov rsp, rbp"
let () = print_endline "  pop rbp"
let () = print_endline "  ret"