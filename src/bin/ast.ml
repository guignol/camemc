
open Printf

type operation = PLUS | MINUS | MUL | DIV
               | EQUAL | NOT_EQUAL | LESS_THAN | LESS_EQUAL | GREATER_THAN | GREATER_EQUAL

type c_type = | TYPE_INT

type typed_name = {
    c_type: c_type;
    name: string
}

type node =
    | Node_No_Op
    | Node_Int of int
    | Node_Binary of operation * node * node
    | Node_Variable of typed_name
    | Node_Assign of node * node
    | Node_Return of node
    | Node_If of node * node * node
    | Node_While of node * node
    | Node_For of node * node * node * node
    | Node_Block of node list
    | Node_Call of string * node list
    | Node_Address of node
    | Node_Deref of node

type global = 
    | Function of typed_name * typed_name list * node list

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

let consume_function tokens arg_consumer = 
    match consume ")" tokens with 
    | Some tokens -> ([], tokens) (* 引数なし *)
    | None -> (* 引数あり *)
        let (arg, tokens) = arg_consumer tokens in
        let rec consume_args args tokens = match consume "," tokens with
            | None -> (args, expect ")" tokens)
            | Some tokens -> 
                let (arg, tokens) = arg_consumer tokens in
                let args = args @ [arg] in
                consume_args args tokens
        in
        consume_args [arg] tokens

(*
program		= function
function   = decl_a "(" params? ")" { stmt* }
stmt		= ("return")? expr ";"
			| decl_a ";"
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
			| "*" unary
			| "&" unary
primary		= num 
			| identifier "(" args? ")"
			| identifier 
			| "(" expr ")"
params		= decl_a ("," decl_a)*
args		= expr ("," expr)*
decl_a     = "int" "*"* identifier
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
    let node_v_declaration tokens = (* TODO 変数宣言 *)
        (* TODO ポインタのポインタ *)
        let tokens = match consume "*" tokens with Some tokens -> tokens | None -> tokens in
        let (_, tokens) = Option.get (consume_identifier tokens) in
        let tokens = expect ";" tokens in
        Node_No_Op, tokens
    in
    match consume "return"	tokens with Some tokens -> node_return tokens | None -> 
    match consume "if"		tokens with Some tokens -> node_if tokens | None -> 
    match consume "while"	tokens with Some tokens -> node_while tokens | None -> 
    match consume "for"		tokens with Some tokens -> node_for tokens | None -> 
    match consume "{"		tokens with Some tokens -> node_block tokens | None -> 
    match consume "int"		tokens with Some tokens -> node_v_declaration tokens | None ->
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
    match consume "&" tokens with Some tokens -> let (n, t) = unary tokens in (Node_Address n, t) | None ->
    match consume "*" tokens with Some tokens -> let (n, t) = unary tokens in (Node_Deref n, t) | None ->
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
            | None -> (Node_Variable { name = name; c_type = TYPE_INT }, tokens)
            | Some tokens -> (* 関数呼び出し *)
                let (args, tokens) = consume_function tokens expr in
                (Node_Call (name, args), tokens)

let function_body typed_name args tokens =
    let tokens = expect "{" tokens in
    let rec body nodes tokens = match consume "}" tokens with
        | Some tokens -> (Function (typed_name, args, nodes), tokens)
        | None ->
            let (node, tokens) = stmt tokens in
            body (nodes @ [node]) tokens
    in
    body [] tokens

let function_definition tokens = 
    let tokens = expect "int" tokens in
    let (name, tokens) = Option.get (consume_identifier tokens) in
    let typed_name = { c_type = TYPE_INT; name = name } in
    let tokens = expect "(" tokens in
    match consume ")" tokens with 
    | Some tokens -> function_body typed_name [] tokens
    | None -> 
        let consume_params tokens = 
            let tokens = expect "int" tokens in
            let (name, tokens) = Option.get (consume_identifier tokens) in
            { c_type = TYPE_INT; name = name }, tokens
        in
        let (params, tokens) = consume_function tokens consume_params in
        function_body typed_name params tokens

let parse tokens =
    let rec parse_globals globals = function
        | [] -> (globals, [])
        | tokens ->
            let (f, tokens) = function_definition tokens in
            parse_globals (globals @ [f]) tokens
    in
    let (globals, tokens) = parse_globals [] tokens in
    let () = if 0 < List.length tokens then
            (* 消費されなかったトークンがあれば出力される *)
            begin
                printf "# [remains] ";
                List.iter Token.print_token tokens;
                print_endline ""
            end
    in
    globals