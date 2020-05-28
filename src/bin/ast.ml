
open Printf

type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type node =
    | Node_Int of int
    | Node_Binary of operation * node * node
    | Node_Variable of string
    | Node_Assign of node * node
    | Node_Return of node

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

let consume str = function
    | [] -> None
    | head :: tail -> match head with
        | Token.Reserved r when r = str -> Some tail
        | _ -> None

let consume_identifier = function
    | [] -> None
    | head :: tail -> match head with
        | Token.Identifier name -> Some (name, tail)
        | _ -> None

let expect str tokens = Option.get (consume str tokens)

let expect_int = function
    | (Token.Number d) :: tokens -> (Node_Int d, tokens)
    | [] -> failwith "tokens are exhausted"
    | t :: _ -> failwith (Token.debug_string_of_token t ^ " is not int")


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
stmt       = ("return")? expr ";"
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
    let (node, tokens) = match consume "return" tokens with 
        | None -> expr tokens 
        | Some tokens -> let (node, tokens) = expr tokens in (Node_Return node, tokens)
    in
    (node, expect ";" tokens)
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
    | Some tokens -> let (node, tokens) = expr tokens in (node, expect ")" tokens)
    | None -> 
        match consume_identifier tokens with
        | Some (name, tokens) -> 
            let node = Node_Variable name in
            (node, tokens)
        | None -> expect_int tokens

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
                List.iter Token.print_token tokens;
                print_endline ""
            end
    in
    nodes