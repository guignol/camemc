
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

(* 文字列リテラル *)
let literals: (('name, 'param, 'meta, 'stack) Global.top_level) list ref = ref []

let add_literal literal = 
    let length = List.length !literals in
    let label = sprintf ".LC.%d" (length + 1) in
    literals :=  !literals @ [Global.String (label, literal)];
    label

let consume_string = function
    | [] -> None
    | head :: tail -> match head with
        | Lexer.String literal -> 
            let label = add_literal literal in
            Some (label, tail)
        | _ -> None

let rec consume_pointer base tokens = match consume "*" tokens with
    | None -> base, tokens
    | Some tokens -> consume_pointer (Type.POINTER base) tokens

let consume_base_type tokens = 
    match consume "int"	tokens	with Some tokens -> Some Type.INT, tokens	| None ->  
    match consume "char" tokens	with Some tokens -> Some Type.CHAR, tokens	| None -> None, tokens 

let rec consume_function_params tokens arg_consumer = 
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

and expect str tokens = 
    try Option.get (consume str tokens) with 
    | Invalid_argument _ ->
        let token_name = match tokens with
            | [] -> "none"
            | t :: _ -> Lexer.debug_string_of_token t
        in
        failwith (str ^ " is expected but " ^ token_name)

let expect_int = function
    | (Lexer.Number d) :: tokens -> d, tokens
    | [] -> failwith "tokens are exhausted"
    | t :: _ -> failwith (Lexer.debug_string_of_token t ^ " is not int")  

let expect_typed_name_with base tokens =
    let c_type, tokens = consume_pointer base tokens in
    let name, tokens = Option.get (consume_identifier tokens) in
    { Type.c_type; Type.name }, tokens 

let expect_typed_name tokens =
    let base, tokens = consume_base_type tokens in
    expect_typed_name_with (Option.get base) tokens

let end_with str parser tokens = 
    let node, tokens = parser tokens in
    node, expect str tokens

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

let discard_result (node, tokens) = Node.Expr_Statement node, tokens

(*
program		= function
global_var = decl_b ";"
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
            | None -> discard_result (end_with ";" expr tokens)
        in
        let condition, tokens = match consume ";" tokens with
            | Some tokens -> Node.Int 1, tokens (* 条件式なし *)
            | None -> end_with ";" expr tokens in
        let iteration, tokens = match consume ")" tokens with
            | Some tokens -> Node.Int 1, tokens (* 反復式なし *)
            | None -> discard_result (end_with ")" expr tokens)
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
    let node_v_declaration base tokens = (* 変数宣言 *)
        let { Type.c_type; Type.name }, tokens = expect_typed_name_with base tokens in
        let rec consume_array c_type tokens = match consume "[" tokens with
            | None -> c_type, tokens
            | Some tokens -> 
                let size, tokens = expect_int tokens in
                let c_type = Type.ARRAY (size, c_type) in
                consume_array c_type (expect "]" tokens)
        in
        let c_type, tokens = consume_array c_type tokens in
        Declaration.add Locally { Type.c_type; Type.name };
        (* TODO 初期化 *)
        Node.Nop, expect ";" tokens
    in
    match consume "return"	tokens with Some tokens -> node_return tokens | None -> 
    match consume "if"		tokens with Some tokens -> node_if tokens | None -> 
    match consume "while"	tokens with Some tokens -> node_while tokens | None -> 
    match consume "for"		tokens with Some tokens -> node_for tokens | None -> 
    match consume "{"		tokens with Some tokens -> node_block tokens | None -> 
    match consume_base_type tokens with (Some t, tokens) -> node_v_declaration t tokens | _ ->
        discard_result (end_with ";" expr tokens)
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
    match consume "sizeof" tokens with Some tokens -> let (n, t) = unary tokens in (Node.SizeOf n, t) | None ->
    match consume "&" tokens with Some tokens -> let (n, t) = unary tokens in (Node.Address n, t) | None ->
    match consume "*" tokens with Some tokens -> let (n, t) = unary tokens in (Node.Deref (NULL, n), t) | None ->
    match consume "+" tokens with Some tokens -> next tokens | None ->
    match consume "-" tokens with
    | Some tokens ->
        let right, tokens = next tokens in
        (* -n = 0 - n *)
        Node.Binary (NULL, Node.MINUS, Node.Int 0, right), tokens
    | None -> next tokens
and accessor tokens =
    let rec with_indexer  (left, tokens) = match consume "[" tokens with
        | None -> left, tokens
        | Some tokens -> 
            let right, tokens = expr tokens in
            (* a[b] は *(a+b) に展開される *)
            (* a[b][c] は *((a + b) + c) に展開される *)
            let node = Node.Indexed (NULL, Node.Binary (NULL, Node.PLUS, left, right)) in
            with_indexer (node, expect "]" tokens)
    in
    with_indexer (primary tokens)
and primary tokens = match consume "(" tokens with
    | Some tokens -> end_with ")" expr tokens
    | None -> match consume_identifier tokens with
        | Some (name, tokens) -> 
            begin match consume "(" tokens with 
            | Some tokens -> (* 関数呼び出し *)
                (* TODO 前方定義の確認 *)
                let args, tokens = consume_function_params tokens expr in
                Node.Call (NULL, name, args), tokens
            | None ->
                (* ローカル変数 *)
                match Declaration.find Locally name with
                | Some ({ Type.name; Type.c_type }, i) -> 
                    Node.Variable (NULL, name, i, Type.is_array c_type), tokens
                | None -> 
                    (* グローバル変数 *)
                    match Declaration.find Globally name with
                    | Some ({ Type.name; _ }, _) -> 
                        Node.Global (NULL, name), tokens
                    | None -> 
                        failwith ("variable " ^ name ^ " is not declared.")
            end
        | None -> 
            match consume_string tokens with
            | Some (label, tokens) -> Node.String label, tokens
            | None -> let d, tokens = expect_int tokens in Node.Int d, tokens

let function_body tokens =
    let tokens = expect "{" tokens in
    let rec body nodes tokens = match consume "}" tokens with
        | None -> let node, tokens = stmt tokens in body (nodes @ [node]) tokens
        | Some tokens -> nodes, tokens
    in
    body [] tokens

let function_params tokens = match consume ")" tokens with 
    | Some tokens -> [], tokens
    | None -> 
        let with_params tokens = 
            let name, tokens = expect_typed_name tokens in
            Declaration.add Locally name;
			name, tokens
        in
        consume_function_params tokens with_params

let parse tokens =
    let rec parse_globals = function
        | [] -> []
        | tokens ->
            let name, tokens = expect_typed_name tokens in
            match consume "(" tokens with
            | Some tokens ->
                (* 関数定義 *)
                Declaration.locals := [];
                let params, tokens = function_params tokens in
                let body, tokens = function_body tokens in
				let locals = !Declaration.locals in
                Global.Function (name, params, body, locals) :: parse_globals tokens
            | None -> 
                (* グローバル変数宣言 *)
                let tokens = expect ";" tokens in
                (* 宣言に追加 *)
				Declaration.add Globally name;
                Global.Variable (NULL, name) :: parse_globals tokens
    in
    let top_levels = parse_globals tokens in
    Declaration.locals := [];
    !literals @ top_levels