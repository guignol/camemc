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
let is_alnum ch = is_alpha ch || (degit_of_char ch) <> None
(* cf. string.ml *)
let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

type token =
  | Number of int
  | Reserved of string
  | Identifier of string

let tokenize reader =
    let rec read_input cursor tokens =
        let buffer ch length =
            let rec get count buffer =
                if length = count then Some buffer else
                match reader (cursor + count) with
                    | None -> None
                    | Some ch -> get (count + 1) (buffer ^ String.make 1 ch)
            in
            get 1 (String.make 1 ch)
        in
        let equals ch target =
            let length = String.length target in
            match buffer ch length with
                | Some str when str = target -> Some (length, target)
                | _ -> None
            (* Keywordの場合、まだ文字が続いている場合は一致しない *)
            (*  let tail_check ch = is_alnum ch in*)
        in
        match reader cursor with None -> tokens | Some ch ->
        (* スペース *)
        if is_space ch then read_input (cursor + 1) tokens else
        (* 記号 *)
        let read_reserved offset r = read_input (cursor + offset) (tokens @ [Reserved r]) in
        let search = List.find_map (equals ch) in
        (* 2文字 *)
        match search ["=="; "!="; "<="; ">=";] with
            Some (_, found) -> read_reserved 2 found | None ->
        (* 1文字 *)
        match search ["="; ";"; "+"; "-"; "*"; "/"; "("; ")"; "<"; ">"] with
            Some (_, found) -> read_reserved 1 found | None ->
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

let debug_string_of_token = function
    | Number d -> sprintf "%d(number)" d
    | Reserved r -> sprintf "%s(reserved)" r
    | Identifier s -> sprintf "%s(identifier)" s

let print_token t = print_string (debug_string_of_token t)

(*let () = List.iter debug_print_token (read 1)*)

(***********************************************************)

type operation = PLUS | MINUS | MUL | DIV
    | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL
let operation_of_string = function
    | "+" -> PLUS
    | "-" -> MINUS
    | "*" -> MUL
    | "/" -> DIV
    | "==" -> EQUAL
    | "!=" -> NOT_EQUAL
    | "<" -> LESS_THAN
    | "<=" -> LESS_EQUAL
    | ">" -> GREATER_THAN
    | ">=" -> GREATER_EQUAL
    | op -> failwith (sprintf "this operation[%s] is not supported" op)

type node =
    | Node_Int of int
    | Node_Binary of operation * node * node
    | Node_Variable of int
    | Node_Assign of node * node

let expect_int = function
    | (Number d) :: tokens -> (Node_Int d, tokens)
    | [] -> failwith "tokens are exhausted"
    | t :: _ -> failwith (debug_string_of_token t ^ " is not int")

let consume str = function
    | [] -> None
    | head :: tail -> match head with
        | Reserved r when r = str -> Some tail
        | _ -> None

let parse_binary_operator tokens next operators =
    let (left, tokens) = next tokens in
    let rec recursive left tokens =
        let rec consume_operator = function
            | [] -> (left, tokens)
            | op_str :: operators -> match consume op_str tokens with
                | Some tokens ->
                    let (right, tokens) = next tokens in
                    let op = operation_of_string op_str in
                    let node = Node_Binary (op, left, right) in
                    recursive node tokens
                | None -> consume_operator operators
        in
        consume_operator operators
    in
    recursive left tokens

(*
program    = stmt*
stmt       = expr ";"
expr       = assign
assign     = equality ("=" assign)?
equality   = relational ("==" relational | "!=" relational)*
relational = add ("<" add | "<=" add | ">" add | ">=" add)*
add        = mul ("+" mul | "-" mul)*
mul        = unary ("*" unary | "/" unary)*
unary      = ("+" | "-")? primary
primary    = num | identifier | "(" expr ")"
*)

let rec stmt tokens =
    let (node, tokens) = expr tokens in
    (node, Option.get (consume ";" tokens))
and expr tokens = assign tokens
and assign tokens =
    let (left, tokens) = equality tokens in
    match consume "=" tokens with None -> (left, tokens) | Some tokens ->
        (* 代入は右結合 *)
        let (right, tokens) = assign tokens in
        let node = Node_Assign (left, right) in
        (node, tokens)
and equality tokens =   parse_binary_operator tokens relational ["=="; "!="]
and relational tokens = parse_binary_operator tokens add        ["<"; "<="; ">"; ">="]
and add tokens =        parse_binary_operator tokens mul        ["+"; "-"]
and mul tokens =        parse_binary_operator tokens unary      ["*"; "/"]
and unary tokens =
    let next tokens = primary tokens in
    match consume "+" tokens with Some tokens -> next tokens | None ->
    match consume "-" tokens with
        | Some tokens ->
            let (right, tokens) = next tokens in
            (* -n = 0 - n *)
            let node = Node_Binary (MINUS, Node_Int 0, right) in
            (node, tokens)
        | None -> next tokens

and primary tokens = match consume "(" tokens with
    | None -> expect_int tokens
    | Some tokens ->
        let (node, tokens) = expr tokens in
        (node, Option.get (consume ")" tokens))

let parse tokens =
    let rec program nodes = function
        | [] -> (nodes, [])
        | tokens ->
            let (new_root_node, tokens) = stmt tokens in
            let nodes = nodes @ [new_root_node] in
            program nodes tokens
    in
    let (nodes, tokens) = program [] tokens in
    let () = if 0 < List.length tokens then
        (* 消費されなかったトークンがあれば出力される *)
        begin
        printf "# [remains] ";
        List.iter print_token tokens;
        print_endline ""
        end
    in
    nodes

(***********************************************************)

let emit_cmp op =
    print_string    "  cmp rax, rdi\n";
    printf          "  %s al\n" op;
    print_string    "  movzb rax, al\n"

let emit_address = function
    | Node_Variable offset ->
        printf          "  lea rax, [rbp - %d]\n" offset;
        print_string    "  push rax\n"
    | _ -> failwith "This node can't emit address."

let load _ =
    print_string    "  pop rax\n";
    (* 8byte == 64bit *)
    print_string    "  mov rax, [rax]\n"

let rec emit = function
    | Node_Variable _ as v ->
        emit_address v;
        load v
    | Node_Assign (left, right) ->
        emit_address left;
        emit right;
        print_string    "  pop rax\n";
        print_string    "  pop rdi\n";
        print_string    "  mov QWORD PTR [rdi], rax\n";
        print_string    "  push rax\n"
    | Node_Int d ->
        printf  "  push %d\n" d;
        (* TODO returnの代替 *)
        printf  "  mov rax, %d\n" d
    | Node_Binary (op, left, right) ->
        emit left;
        emit right;
        print_string    "  pop rdi\n";
        print_string    "  pop rax\n";
        begin match op with
        | PLUS -> print_string  "  add rax, rdi\n";
        | MINUS -> print_string "  sub rax, rdi\n";
        | MUL -> print_string   "  imul rax, rdi\n";
        | DIV ->
            (*
            cqo命令を使うと、RAXに入っている64ビットの値を128ビットに伸ばしてRDXとRAXにセットする
            idivは暗黙のうちにRDXとRAXを取って、それを合わせたものを128ビット整数とみなして、
            それを引数のレジスタの64ビットの値で割り、商をRAXに、余りをRDXにセットする
            *)
            print_string   "  cqo\n";
            print_string   "  idiv rdi\n";
        | EQUAL         -> emit_cmp "sete"
        | NOT_EQUAL     -> emit_cmp "setne"
        | LESS_THAN     -> emit_cmp "setl"
        | LESS_EQUAL    -> emit_cmp "setle"
        | GREATER_THAN  -> emit_cmp "setg"
        | GREATER_EQUAL -> emit_cmp "setge"
        end;
        print_string   "  push rax\n"

let () =
    let input = Sys.argv.(1) in
    let reader index = try Some input.[index] with _ -> None in
    let tokens = tokenize reader in
    let trees = parse tokens in
    print_endline  ".intel_syntax noprefix";
    print_endline  ".text";
    print_endline  ".global main";
    print_endline  "main:";
    (* プロローグ *)
    print_endline  "  push rbp";
    print_endline  "  mov rbp, rsp";
    List.iter emit trees;
    print_endline  ".Lreturn.main:";
    (* エピローグ *)
    print_endline  "  mov rsp, rbp";
    print_endline  "  pop rbp";
    print_endline  "  ret";