
open Printf

type nothing = NULL

let operation_of_string = function
    | "+" -> Node.PLUS
    | "-" -> Node.MINUS
    | "*" -> Node.MUL
    | "/" -> Node.DIV
    | "==" -> Node.EQUAL
    | "!=" -> Node.NOT_EQUAL
    | "<" -> Node.LESS_THAN
    | "<=" -> Node.LESS_EQUAL
    | ">" -> Node.GREATER_THAN
    | ">=" -> Node.GREATER_EQUAL
    | op -> failwith (sprintf "this operation[%s] is not supported here." op)

let consume str = function
    | [] -> None
    | head :: tail -> match head with
        | Lexer.Reserved r when r = str -> Some tail
        | _ -> None

let consume_identifier = function
    | [] -> None
    | head :: tail -> match head with
        | Lexer.Identifier name -> Some (name, tail)
        | _ -> None

let expect str tokens = 
    try Option.get (consume str tokens) 
    with Invalid_argument _ ->
        let token_name = match tokens with
            | [] -> "none"
            | t :: _ -> Lexer.debug_string_of_token t
        in
        failwith (str ^ " is expected but " ^ token_name)

let end_with str parser tokens = 
    let node, tokens = parser tokens in
    node, expect str tokens

let expect_int = function
    | (Lexer.Number d) :: tokens -> d, tokens
    | [] -> failwith "tokens are exhausted"
    | t :: _ -> failwith (Lexer.debug_string_of_token t ^ " is not int")

let binary tokens next operators =
    let (left, tokens) = next tokens in
    let rec recursive left tokens =
        let rec consume_operator = function
            | [] -> left, tokens
            | op_str :: operators -> match consume op_str tokens with
                | Some tokens ->
                    let right, tokens = next tokens in
                    let op = operation_of_string op_str in
                    let node = Node.Binary (NULL, op, left, right) in
                    recursive node tokens
                | None -> consume_operator operators
        in
        consume_operator operators
    in
    recursive left tokens

let as_statement (node, tokens) = Node.Expr_Statement (NULL, node), tokens

let function_params tokens arg_consumer = 
    match consume ")" tokens with 
    | Some tokens -> [], tokens (* 引数なし *)
    | None -> (* 引数あり *)
        let (arg, tokens) = arg_consumer tokens in
        let rec consume_args args tokens = match consume "," tokens with
            | None -> (args, expect ")" tokens)
            | Some tokens -> 
                let arg, tokens = arg_consumer tokens in
                let args = args @ [arg] in
                consume_args args tokens
        in
        consume_args [arg] tokens

(* 変数宣言 *)
let locals: Type.typed_name list ref = ref []
let find_variables_by_name name =
    let rec find index = function
        | [] -> None
        | head :: tail -> 
            if head.Type.name = name 
            then Some (head, index)
            else find (index + 1) tail
    in
    find 0 !locals
let add_declaration c_type name tokens =
    match find_variables_by_name name with
    | Some _ -> failwith ("variable " ^ name ^ " is already declared.")
    | None ->
        let d = { Type.name; Type.c_type } in
        locals := !locals @ [d];
        d, tokens

(*
program		= function
function   = decl_a "(" params? ")" { stmt* }
stmt		= ("return")? expr ";"
			| decl_b ";"
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
unary		= "sizeof" unary
			| "*" unary
			| "&" unary
			| ("+" | "-")? unary
			| accessor
accessor	= primary ("[" expr "]")*
primary		= num
			| identifier
			| identifier "(" args? ")"
			| "(" expr ")"
params		= decl_a ("," decl_a)*
args		= expr ("," expr)*
base_type	= "int"
decl_a		= base_type "*"* identifier
decl_b		= decl_a ("[" num "]")*

https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
*)

let rec stmt tokens =
    let node_return tokens = 
        let node, tokens = expr tokens in
        Node.Return node, expect ";" tokens
    in
    let node_if tokens = 
        let tokens = expect "(" tokens in
        let condition, tokens = end_with ")" expr tokens in
        let if_true, tokens = stmt tokens in
        let if_false, tokens = match consume "else" tokens with
            | Some tokens -> stmt tokens
            | None -> Node.Nop, tokens in
        Node.If (condition, if_true, if_false), tokens
    in
    let node_while tokens = 
        let tokens = expect "(" tokens in
        let condition, tokens = end_with ")" expr tokens in
        let execution, tokens = stmt tokens in
        Node.While (condition, execution), tokens
    in
    let node_for tokens =
        let tokens = expect "(" tokens in
        let init, tokens = match consume ";" tokens with
            | Some tokens -> Node.Int 1, tokens (* 初期化式なし *)
            | None -> as_statement (end_with ";" expr tokens)
        in
        let condition, tokens = match consume ";" tokens with
            | Some tokens -> Node.Int 1, tokens (* 条件式なし *)
            | None -> end_with ";" expr tokens in
        let iteration, tokens = match consume ")" tokens with
            | Some tokens -> Node.Int 1, tokens (* 反復式なし *)
            | None -> as_statement (end_with ")" expr tokens)
        in
        let execution, tokens = stmt tokens in
        Node.For (init, condition, iteration, execution), tokens
    in
    let node_block tokens =
        let rec node_block_rec tokens list = match consume "}" tokens with
            | Some tokens -> list, tokens
            | None -> 
                let stmt, tokens = stmt tokens in
                node_block_rec tokens (list @ [stmt])
        in
        let nodes, tokens = node_block_rec tokens [] in
        Node.Block nodes, tokens
    in
    let node_v_declaration tokens = (* 変数宣言 *)
        (* TODO ポインタのポインタ *)
        let c_type, tokens = match consume "*" tokens with
            | None -> Type.INT, tokens
            | Some tokens -> (Type.POINTER Type.INT), tokens
        in
        let name, tokens = Option.get (consume_identifier tokens) in
        (* TODO 配列の配列 *)
        let c_type, tokens = match consume "[" tokens with
            | None -> c_type, tokens
            | Some tokens -> 
                let d, tokens = expect_int tokens in
                Type.ARRAY (d, c_type), expect "]" tokens
        in
        let _, tokens = add_declaration c_type name tokens in
        Node.Nop, expect ";" tokens
    in
    match consume "return"	tokens with Some tokens -> node_return tokens | None -> 
    match consume "if"		tokens with Some tokens -> node_if tokens | None -> 
    match consume "while"	tokens with Some tokens -> node_while tokens | None -> 
    match consume "for"		tokens with Some tokens -> node_for tokens | None -> 
    match consume "{"		tokens with Some tokens -> node_block tokens | None -> 
    match consume "int"		tokens with Some tokens -> node_v_declaration tokens | None ->
        as_statement (end_with ";" expr tokens)
and expr tokens = assign tokens
and assign tokens =
    let left, tokens = equality tokens in
    match consume "=" tokens with None -> (left, tokens) | Some tokens ->
        let right, tokens = assign tokens in (* 代入は右結合 *)
        Node.Assign (NULL, left, right), tokens
and equality tokens =   binary tokens relational ["=="; "!="]
and relational tokens = binary tokens add        ["<"; "<="; ">"; ">="]
and add tokens =        binary tokens mul        ["+"; "-"]
and mul tokens =        binary tokens unary      ["*"; "/"]
and unary tokens =
    let next tokens = accessor tokens in
    (* TODO sizeof ( int * ) *)
    match consume "sizeof" tokens with Some tokens -> let (n, t) = unary tokens in (Node.SizeOf n, t) | None ->
    match consume "&" tokens with Some tokens -> let (n, t) = unary tokens in (Node.Address (NULL, n), t) | None ->
    match consume "*" tokens with Some tokens -> let (n, t) = unary tokens in (Node.Deref (NULL, n), t) | None ->
    match consume "+" tokens with Some tokens -> next tokens | None ->
    match consume "-" tokens with
    | Some tokens ->
        let right, tokens = next tokens in
        (* -n = 0 - n *)
        Node.Binary (NULL, Node.MINUS, Node.Int 0, right), tokens
    | None -> next tokens
and accessor tokens =
	let rec with_indexer (left, tokens) = match consume "[" tokens with
	| None -> left, tokens
	| Some tokens -> 
		let right, tokens = expr tokens in
		(* a[b] は *(a+b) に展開される *)
		let node = Node.Deref (NULL, Node.Binary (NULL, Node.PLUS, left, right)) in
		with_indexer (node, expect "]" tokens)
	in
	with_indexer (primary tokens)
and primary tokens = match consume "(" tokens with
    | Some tokens -> end_with ")" expr tokens
    | None -> 
        match consume_identifier tokens with
        | None -> let d, tokens = expect_int tokens in Node.Int d, tokens
        | Some (name, tokens) -> match consume "(" tokens with 
            | Some tokens -> (* 関数呼び出し *)
                let args, tokens = function_params tokens expr in
                Node.Call (NULL, name, args), tokens
            | None ->
                match find_variables_by_name name with
                | None -> failwith ("variable " ^ name ^ " is not declared.")
                | Some ({ Type.name; Type.c_type }, i) -> 
                    Node.Variable (NULL, name, i, Type.is_array c_type), tokens

let function_body returned params tokens =
    let tokens = expect "{" tokens in
    let rec body nodes tokens = match consume "}" tokens with
        | None -> let node, tokens = stmt tokens in body (nodes @ [node]) tokens
        | Some tokens -> 
            Node.Function (returned, params, nodes, !locals), tokens
    in
    body [] tokens

let function_definition tokens = 
    locals := [];
    let tokens = expect "int" tokens in
    let name, tokens = Option.get (consume_identifier tokens) in
    let c_type = Type.INT in
    let returned = { Type.c_type; Type.name} in
    let tokens = expect "(" tokens in
    match consume ")" tokens with 
    | Some tokens -> function_body returned [] tokens
    | None -> 
        let with_params tokens = 
            let tokens = expect "int" tokens in
            let name, tokens = Option.get (consume_identifier tokens) in
            add_declaration Type.INT name tokens
        in
        let params, tokens = function_params tokens with_params in
        function_body returned params tokens

let parse tokens =
    let rec parse_globals globals = function
        | [] -> globals, []
        | tokens ->
            let f, tokens = function_definition tokens in
            parse_globals (globals @ [f]) tokens
    in
    let globals, tokens = parse_globals [] tokens in
    let () = if 0 < List.length tokens then
            (* 消費されなかったトークンがあれば出力される *)
            begin
                printf "# [remains] ";
                List.iter Lexer.print_token tokens;
                print_endline ""
            end
    in
    globals