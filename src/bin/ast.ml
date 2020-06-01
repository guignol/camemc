
open Printf

type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type node =
    | Node_No_Op
    | Node_Int of int
    | Node_Binary of operation * node * node
    | Node_Variable of string
    | Node_Assign of node * node
    | Node_Return of node
    | Node_If of node * node * node
    | Node_While of node * node
    | Node_For of node * node * node * node
    | Node_Block of node list
    | Node_Call of string * node list

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

let expect str tokens = 
    try Option.get (consume str tokens) 
    with Invalid_argument _ ->
        let token_name = match tokens with
            | [] -> "none"
            | t :: _ -> Token.debug_string_of_token t
        in
        failwith (str ^ " is expected but " ^ token_name)

let end_with str parser tokens = 
    let (node, tokens) = parser tokens in
    (node, expect str tokens)

let expect_int = function
    | (Token.Number d) :: tokens -> (Node_Int d, tokens)
    | [] -> failwith "tokens are exhausted"
    | t :: _ -> failwith (Token.debug_string_of_token t ^ " is not int")

let binary tokens next operators =
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
program		= stmt*
stmt		= ("return")? expr ";"
        	| "{" stmt* "}"
	        | "if" "(" expr ")" stmt ("else" stmt)?
			| "while" "(" expr ")" stmt
        	| "for" "(" expr? ";" expr? ";" expr? ")" stmt
expr		= assign
assign		= equality ("=" assign)?
equality	= relational ("==" relational | "!=" relational)*
relational	= add ("<" add | "<=" add | ">" add | ">=" add)*
add			= mul ("+" mul | "-" mul)*
mul			= unary ("*" unary | "/" unary)*
unary		= ("+" | "-")? primary
primary		= num 
			| identifier "(" args? ")"
			| identifier 
			| "(" expr ")"
args		= expr ("," expr)*
*)

let rec stmt tokens =
    let node_return tokens = 
        let (node, tokens) = expr tokens in
        (Node_Return node, expect ";" tokens)
    in
    let node_if tokens = 
        let tokens = expect "(" tokens in
        let (condition, tokens) = end_with ")" expr tokens in
        let (if_true, tokens) = stmt tokens in
        let (if_false, tokens) = match consume "else" tokens with
            | Some tokens -> stmt tokens
            | None -> (Node_No_Op, tokens) in
        (Node_If (condition, if_true, if_false), tokens)
    in
    let node_while tokens = 
        let tokens = expect "(" tokens in
        let (condition, tokens) = end_with ")" expr tokens in
        let (execution, tokens) = stmt tokens in
        (Node_While (condition, execution), tokens)
    in
    let node_for tokens =
        let tokens = expect "(" tokens in
        let (init, tokens) = match consume ";" tokens with
            | Some tokens -> (Node_Int 1, tokens) (* 初期化式なし *)
            | None -> end_with ";" expr tokens in
        let (condition, tokens) = match consume ";" tokens with
            | Some tokens -> (Node_Int 1, tokens) (* 条件式なし *)
            | None -> end_with ";" expr tokens in
        let (iteration, tokens) = match consume ")" tokens with
            | Some tokens -> (Node_Int 1, tokens) (* 反復式なし *)
            | None -> end_with ")" expr tokens in
        let (execution, tokens) = stmt tokens in
        (Node_For (init, condition, iteration, execution), tokens)
    in
    let node_block tokens =
        let rec node_block_rec tokens list = match consume "}" tokens with
            | Some tokens -> (list, tokens)
            | None -> 
                let (stmt, tokens) = stmt tokens in
                node_block_rec tokens (list @ [stmt])
        in
        let (nodes, tokens) = node_block_rec tokens [] in
        (Node_Block nodes, tokens)
    in
    match consume "return" tokens with | Some tokens -> node_return tokens | None -> 
    match consume "if" tokens with | Some tokens -> node_if tokens | None -> 
    match consume "while" tokens with | Some tokens -> node_while tokens | None -> 
    match consume "for" tokens with | Some tokens -> node_for tokens | None -> 
    match consume "{" tokens with | Some tokens -> node_block tokens | None -> 
        end_with ";" expr tokens
and expr tokens = assign tokens
and assign tokens =
    let (left, tokens) = equality tokens in
    match consume "=" tokens with None -> (left, tokens) | Some tokens ->
        let (right, tokens) = assign tokens in (* 代入は右結合 *)
        (Node_Assign (left, right), tokens)
and equality tokens =   binary tokens relational ["=="; "!="]
and relational tokens = binary tokens add        ["<"; "<="; ">"; ">="]
and add tokens =        binary tokens mul        ["+"; "-"]
and mul tokens =        binary tokens unary      ["*"; "/"]
and unary tokens =
    let next tokens = primary tokens in
    match consume "+" tokens with Some tokens -> next tokens | None ->
    match consume "-" tokens with
    | Some tokens ->
        let (right, tokens) = next tokens in
        (* -n = 0 - n *)
        (Node_Binary (MINUS, Node_Int 0, right), tokens)
    | None -> next tokens
and primary tokens = match consume "(" tokens with
    | Some tokens -> end_with ")" expr tokens
    | None -> 
        match consume_identifier tokens with
        | None -> expect_int tokens
        | Some (name, tokens) -> match consume "(" tokens with 
            | None -> (Node_Variable name, tokens)
            | Some tokens -> 
                match consume ")" tokens with 
                | Some tokens -> (Node_Call (name, []), tokens)
                | None -> 
					let (arg, tokens) = expr tokens in
                    let rec consume_args args tokens = match consume "," tokens with
                        | None -> (Node_Call (name, args), expect ")" tokens)
                        | Some tokens -> 
                            let (arg, tokens) = expr tokens in
                            let args = args @ [arg] in
                            consume_args args tokens
                    in
                    consume_args [arg] tokens

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