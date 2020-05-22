(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf
open String

(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)

type operation = Plus | Minus | Mul | Div

type token =
    | Number of int
    | Operator of operation
    | Identifier of string

let starts_with str ch = match index_opt str ch with
    | Some index -> index = 0
    | None -> false

let rec starts_with_any_of_list str = function
    | [] -> false
    | head :: tail -> starts_with str head || starts_with_any_of_list str tail

let numbers = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']

let is_number ch = List.exists (fun c -> c = ch) numbers

let starts_with_number str = starts_with_any_of_list str numbers

let debug_print_token = function
    | Number d -> printf "# int: %d\n" d
    | Operator op ->
        begin
        match op with
            | Plus -> printf "# operator: %s\n" "+"
            | Minus -> printf "# operator: %s\n" "-"
            | Mul -> printf "# operator: %s\n" "*"
            | Div -> printf "# operator: %s\n" "/"
        end
    | Identifier s -> printf "# identifier: %s\n" s

(* cf. string.ml *)
let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let tokenize reader =
    let rec read_input cursor tokens =
        match reader cursor with None -> tokens | Some ch ->
        (* スペース *)
        if is_space ch then read_input (cursor + 1) tokens else
        (* 四則演算 *)
        let read_operation op = read_input (cursor + 1) (tokens @ [Operator op]) in
        match ch with
            | '+' -> read_operation Plus
            | '-' -> read_operation Minus
            | '*' -> read_operation Mul
            | '/' -> read_operation Div
            | _ ->
        (* 数値 *)
        let read_number offset d = read_input (cursor + offset) (tokens @ [Number d]) in
        let rec to_number offset str = match reader (cursor + offset) with
            | None -> (offset, str)
            | Some ch ->
                if is_number ch
                then to_number (offset + 1) (str ^ String.make 1 ch)
                else (offset, str)
        in
        if is_number ch
        then let (offset, number) = to_number 1 (String.make 1 ch) in read_number offset (int_of_string number) else
        (* その他 *)
        read_input (cursor + 1) (tokens @ [Identifier (String.make 1 ch)])
    in
    read_input 0 []

(*let () = List.iter debug_print_token (read 1)*)

(***********************************************************)

type node =
    | Node_Int of int
    | Node_Calc of operation * node * node

let expect_int = function
    (Number d) :: tokens -> (Node_Int d, tokens)
    | _ -> failwith "this is not int"

(*
expr    = add
add     = mul ("+" mul | "-" mul)*
mul     = primary ("*" primary | "/" primary)*
primary = num
↑まずこれ
↓あとで
primary = num | "(" expr ")"
*)

let mul tokens =
    let (left, tokens) = expect_int tokens in
    let rec mul_inner left = function
        | [] -> (left, [])
        | head :: tail -> match head with
            | Operator op ->
                begin match op with
                | Mul | Div ->
                    let (right, tail) = expect_int tail in
                    let node = Node_Calc (op, left, right) in
                    mul_inner node tail
                | _ -> (left, head :: tail)
                end
            | _ -> (left, head :: tail) in
    mul_inner left tokens

let add tokens =
    let (left, tokens) = mul tokens in
    let rec add_inner left = function
        | [] -> (left, [])
        | head :: tail -> match head with
            | Operator op ->
                begin match op with
                | Plus | Minus ->
                    let (right, tail) = mul tail in
                    let node = Node_Calc (op, left, right) in
                    add_inner node tail
                | _ -> (left, head :: tail)
                end
            | _ -> (left, head :: tail) in
    add_inner left tokens

let rec debug_print_ast = function
    | Node_Int d -> printf " %d" d
    | Node_Calc (op, left, right) ->
        debug_print_ast left;
        begin match op with
        | Plus -> printf " +"
        | Minus -> printf " -"
        | Mul -> printf " *"
        | Div -> printf " /"
        end;
        debug_print_ast right

let parse tokens =
    let (node, _) = add tokens in
    let () =
        printf "#";
        debug_print_ast node;
        print_endline ""
    in
    node

(***********************************************************)

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
        | Mul -> print_string   "  imul rax, rdi\n";
        (*
        cqo命令を使うと、RAXに入っている64ビットの値を128ビットに伸ばしてRDXとRAXにセットする
        idivは暗黙のうちにRDXとRAXを取って、それを合わせたものを128ビット整数とみなして、
        それを引数のレジスタの64ビットの値で割り、商をRAXに、余りをRDXにセットする
        *)
        | Div ->
            print_string   "  cqo\n";
            print_string   "  idiv rdi\n";
        end;
        print_string   "  push rax\n"

let () =
    let input = Sys.argv.(1) in
    let reader index = try Some input.[index] with _ -> None in
    let tokens = tokenize reader in
    let ast = parse tokens in
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