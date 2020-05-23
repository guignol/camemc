(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf
open String

(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)

let starts_with str ch = match index_opt str ch with
    | Some index -> index = 0
    | None -> false

let degit_of_char = function
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alnum ch = is_alpha ch || (degit_of_char ch) != None
(* cf. string.ml *)
let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

type token =
  | Number of int
  | Reserved of string
  | Identifier of string

let tokenize reader =
    let rec read_input cursor tokens =
        let equals ch str =
            let length = String.length str in
            if length = 0 then None else
            let rec check offset buffer =
                match reader (cursor + offset) with
                    | None -> None
                    | Some ch ->
                        if length = offset then
                            (* まだ文字が続いている場合は一致しない *)
                            let continues = is_alnum ch in
                            if str = buffer && not continues then Some (offset, str) else None
                        else check (offset + 1) (buffer ^ String.make 1 ch)
            in
            check 1 (String.make 1 ch)
        in
        match reader cursor with None -> tokens | Some ch ->
        (* スペース *)
        if is_space ch then read_input (cursor + 1) tokens else
        (* 2文字以上の記号 *)
        let read_reserved offset r = read_input (cursor + offset) (tokens @ [Reserved r]) in
        let found = List.find_map (equals ch) ["=="; "!="] in
        match found with Some (offset, target) -> read_reserved offset target | None ->
        (* 1文字の記号 *)
        match ch with '+' | '-' | '*' | '/' | '(' | ')' -> read_reserved 1 (String.make 1 ch) | _ ->
        (* 数値 *)
        let read_number offset d = read_input (cursor + offset) (tokens @ [Number d]) in
        let rec concat_number offset number = match reader (cursor + offset) with
            | None -> (offset, number)
            | Some ch ->match degit_of_char ch with
                | None -> (offset, number)
                | Some n -> concat_number (offset + 1) (number * 10 + n)
        in
        let (offset, number) = concat_number 0 0 in
        if 0 < offset then read_number offset number else
        (* その他 *)
        read_input (cursor + 1) (tokens @ [Identifier (String.make 1 ch)])
    in
    read_input 0 []

let debug_print_token = function
    | Number d -> printf "# int: %d\n" d
    | Reserved r -> printf "# %s\n" r
    | Identifier s -> printf "# identifier: %s\n" s

(*let () = List.iter debug_print_token (read 1)*)

(***********************************************************)

type operation = Plus | Minus | Mul | Div | Equal | Not_Equal
let from_token = function
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Mul
    | "/" -> Div
    | "==" -> Equal
    | "!=" -> Not_Equal
    | op -> failwith (sprintf "this operation[%s] is not supported" op)

type node =
    | Node_Int of int
    | Node_Binary of operation * node * node

let expect_int = function
    (Number d) :: tokens -> (Node_Int d, tokens)
    | _ -> failwith "this is not int"

let expect ~next = function
    | None -> failwith "something's lost"
    | Some s -> next (s)

let consume str = function
    | [] -> None
    | head :: tail -> match head with
        | Reserved r when r = str -> Some tail
        | _ -> None

let parse_binary_operator tokens next operators =
    let (left, tokens) = next tokens in
    let rec recursive left tokens =
        let binary_node op tokens =
            let (right, tokens) = next tokens in
            let node = Node_Binary (op, left, right) in
            recursive node tokens in
        let rec consume_operator = function
            | [] -> (left, tokens)
            | op_str :: operators ->
                match consume op_str tokens with
                    | Some tokens -> binary_node (from_token op_str) tokens
                    | None -> consume_operator operators
        in
        consume_operator operators
    in
    recursive left tokens

(*
expr       = equality
equality   = relational ("==" relational | "!=" relational)*
relational = add ("<" add | "<=" add | ">" add | ">=" add)*
add        = mul ("+" mul | "-" mul)*
mul        = unary ("*" unary | "/" unary)*
unary      = ("+" | "-")? primary
primary    = num | "(" expr ")"
*)

let rec expr tokens =   equality tokens
and equality tokens =   parse_binary_operator tokens relational ["=="; "!="]
and relational tokens = parse_binary_operator tokens add        ["<"; "<="; ">"; ">="]
and add tokens =        parse_binary_operator tokens mul        ["+"; "-"]
and mul tokens =        parse_binary_operator tokens unary      ["*"; "/"]
and unary tokens =
    let next tokens = primary tokens in
    match consume "-" tokens with
        | Some tokens ->
            let (right, tokens) = next tokens in
            (* -n = 0 - n *)
            let node = Node_Binary (Minus, Node_Int 0, right) in
            (node, tokens)
        | None ->
    match consume "+" tokens with
        | Some tokens -> next tokens
        | None -> next tokens

and primary tokens = match consume "(" tokens with
    | None -> expect_int tokens
    | Some tokens ->
        let (node, tokens) = expr tokens in
        let continue tokens = (node, tokens) in
        expect (consume ")" tokens) ~next:continue

let parse tokens =
    let (nodes, _) = expr tokens in
(*    let () =*)
(*        printf "#";*)
(*        debug_print_ast nodes;*)
(*        print_endline ""*)
(*    in*)
    nodes

(***********************************************************)

let rec emit = function
    | Node_Int d -> printf "  push %d\n" d
    | Node_Binary (op, left, right) ->
        emit left;
        emit right;
        print_string   "  pop rdi\n";
        print_string   "  pop rax\n";
        begin match op with
        | Plus -> print_string   "  add rax, rdi\n";
        | Minus -> print_string   "  sub rax, rdi\n";
        | Mul -> print_string   "  imul rax, rdi\n";
        | Div ->
            (*
            cqo命令を使うと、RAXに入っている64ビットの値を128ビットに伸ばしてRDXとRAXにセットする
            idivは暗黙のうちにRDXとRAXを取って、それを合わせたものを128ビット整数とみなして、
            それを引数のレジスタの64ビットの値で割り、商をRAXに、余りをRDXにセットする
            *)
            print_string   "  cqo\n";
            print_string   "  idiv rdi\n";
        | Equal ->
            print_string   "  cmp rax, rdi\n";
            print_string   "  sete al\n";
            print_string   "  movzb rax, al\n";
        | Not_Equal ->
            print_string   "  cmp rax, rdi\n";
            print_string   "  setne al\n";
            print_string   "  movzb rax, al\n";
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