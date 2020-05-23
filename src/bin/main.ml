(*let () = print_endline "Hello, World!"*)
(*let () = print_endline "Bye!"*)

open Printf
open String

(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)

type token =
    | Number of int
    | Reserved of string
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

(* cf. string.ml *)
let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let tokenize reader =
    let rec read_input cursor tokens =
        match reader cursor with None -> tokens | Some ch ->
        (* スペース *)
        if is_space ch then read_input (cursor + 1) tokens else
        (* 記号 *)
        let read_reserved r = read_input (cursor + 1) (tokens @ [Reserved r]) in
        match ch with
            | '+' | '-' | '*' | '/' | '(' | ')' -> read_reserved (String.make 1 ch)
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

let debug_print_token = function
    | Number d -> printf "# int: %d\n" d
    | Reserved r -> printf "# %s\n" r
    | Identifier s -> printf "# identifier: %s\n" s

(*let () = List.iter debug_print_token (read 1)*)

(***********************************************************)

type operation = Plus | Minus | Mul | Div | EQUAL

type node =
    | Node_Int of int
    | Node_Binary of operation * node * node

let expect_int = function
    (Number d) :: tokens -> (Node_Int d, tokens)
    | _ -> failwith "this is not int"

let expect condition ~next = match condition with
    | None -> failwith "something's lost"
    | Some s -> next (s)

let consume str = function
    | [] -> None
    | head :: tail -> match head with
        | Reserved r when r = str -> Some tail
        | _ -> None

(*
expr       = equality
equality   = relational ("==" relational | "!=" relational)*
relational = add ("<" add | "<=" add | ">" add | ">=" add)*
add        = mul ("+" mul | "-" mul)*
mul        = unary ("*" unary | "/" unary)*
unary      = ("+" | "-")? primary
primary    = num | "(" expr ")"
*)

let rec expr tokens = add tokens

(*and expr tokens = equality tokens*)

(*and equality tokens =*)
(*    let (left, tokens) = relational tokens in*)
(*    let rec equality_inner left = function*)
(*        | [] -> (left, [])*)
(*        | _ -> match consume "==" tokens with*)
(*            | Some tokens ->*)
(*                    let (right, tokens) = relational tokens in*)
(*                    let node = Node_Binary (op, left, right) in*)
(*            | None ->*)

(*and relational tokens = add tokens*)

and add tokens =
    let next tokens = mul tokens in
    let (left, tokens) = next tokens in
    let rec recursive left tokens =
       let binary_node op tokens =
           let (right, tokens) = next tokens in
           let node = Node_Binary (op, left, right) in
           recursive node tokens in
       match consume "+" tokens with
           | Some tokens -> binary_node Plus tokens
           | None ->
       match consume "-" tokens with
           | Some tokens -> binary_node Minus tokens
           | None -> (left, tokens)
    in
    recursive left tokens

and mul tokens =
    let next tokens = unary tokens in
    let (left, tokens) = next tokens in
    let rec recursive left tokens =
        let binary_node op tokens =
            let (right, tokens) = next tokens in
            let node = Node_Binary (op, left, right) in
            recursive node tokens in
        match consume "*" tokens with
            | Some tokens -> binary_node Mul tokens
            | None ->
        match consume "/" tokens with
            | Some tokens -> binary_node Div tokens
            | None -> (left, tokens)
    in
    recursive left tokens

and unary tokens =
    let next tokens = primary tokens in
    match consume "-" tokens with
        | Some tokens ->
            let (right, tokens) = next tokens in
            (* -m = 0 - n *)
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
        | EQUAL ->
            (* TODO *)
            print_string   "  \n";
            print_string   "  \n";
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