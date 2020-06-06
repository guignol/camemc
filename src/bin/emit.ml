
open Printf
open Node

let context = ref 0

let registers_64 = ["rdi";
                    "rsi";
                    "rdx";
                    "rcx";
                    "r8";
                    "r9"]
let registers_32 = ["edi";
                    "esi";
                    "edx";
                    "ecx";
                    "r8d";
                    "r9d"]
let registers_8 = ["DIL";
                   "SIL";
                   "DL";
                   "CL";
                   "R8B";
                   "R9B"]

let size_prefix = function
    | 1 -> "BYTE PTR"	(* 1byte == 8bit *)
    | 4 -> "DWORD PTR"	(* 4byte == 32bit *)
    | 8 -> "QWORD PTR"	(* 8byte == 64bit *)
    | size -> failwith (sprintf "[size_prefix]%d bit regsiter is not supported" (size * 8))
let register_name_rax = function
    | 1 -> "al"		(* 1byte == 8bit *)
    | 4 -> "eax"	(* 4byte == 32bit *)
    | 8 -> "rax"	(* 8byte == 64bit *)
    | size -> failwith (sprintf "[register_name_rax]%d bit regsiter is not supported" (size * 8))
let register_name_for_parameter index = function
    | 1 -> List.nth registers_8 index	(* 1byte == 8bit *)
    | 4 -> List.nth registers_32 index	(* 4byte == 32bit *)
    | 8 -> List.nth registers_64 index	(* 8byte == 64bit *)
    | size -> failwith (sprintf "[register_name_for_parameter]%d bit regsiter is not supported" (size * 8))

let emit_cmp op =
    print_string    "  cmp rax, rdi\n";
    printf          "  %s al\n" op;
    print_string    "  movzb rax, al\n"

let load size =
    print_string    "  pop rax\n";
    let prefix = size_prefix size in
    let _ = match size with
        | 1 -> (* 1byte == 8bit *)
            printf			"  movsx rax, %s [rax]\n" prefix
        | 4 -> (* 4byte == 32bit *)
            (* 負の値がcmpでゼロ埋め拡張されないように、符号拡張する *)
            printf			"  movsxd rax, %s [rax]\n" prefix
        | 8-> (* 8byte == 64bit *)
            print_string	"  mov rax, [rax]\n"
        | _ -> failwith ""
    in
    print_string    "  push rax\n"

let emit node = 
    let rec emit_address = function
        | Node_Variable (_, name, offset) ->
            printf          "  # variable [%s]\n" name;
            printf          "  lea rax, [rbp - %d]\n" offset;
            print_string    "  push rax\n"
        | Node_Deref (_, node) -> emit_inner node
        | _ -> failwith "This node can't emit address."
    and emit_inner = function
        | Node_Address (_, node) -> 
            (* 変数のアドレスをスタックに積むだけ *)
            emit_address node
        | Node_Deref (size, node) ->
            (* ポインタ変数の値（アドレス）をスタックに積む *)
            emit_inner node;
            (* それをロードする *)
            load size
        | Node_Call (_, name, args) -> 
            List.iter emit_inner args;
            let count = List.length args in
            let pop i _ = 
                let register = List.nth registers_64 (count - i - 1) in
                printf		"  pop %s\n" register
            in
            List.iteri pop args;
            (* https://github.com/rui314/chibicc/commit/aedbf56c3af4914e3f183223ff879734683bec73 *)
            (* We need to align RSP to a 16 byte boundary before *)
            (* calling a function because it is an ABI requirement. *)
            (* RAX is set to 0 for variadic function. *)
            let context = incr context; !context in
            printf			"  mov rax, rsp\n";
            printf			"  and rax, 0xF\n";		
            printf			"  jnz .Lcall%d\n" context;		
            printf			"  mov rax, 0\n";		
            printf			"  call %s\n" name;		
            printf			"  jmp .Lend%d\n" context;		
            printf			".Lcall%d:\n" context;		
            printf			"  sub rsp, 8\n";		
            printf			"  mov rax, 0\n";		
            printf			"  call %s\n" name;		
            printf			"  add rsp, 8\n";		
            printf			".Lend%d:\n" context;		
            printf			"  push rax\n"
        | Node_No_Op -> 
            print_string	"  # no op\n"
        | Node_Block nodes ->
            List.iter emit_inner nodes
        | Node_For (init, condition, iteration, execution) ->
            let context = incr context; !context in
            (* init *)
            emit_inner init;
            print_string	"  pop rax\n";
            (* begin *)
            printf			".Lcondition%d:\n" context;
            (* condition *)
            emit_inner condition;
            (* if 0, goto end *)
            print_string	"  pop rax\n";
            print_string	"  cmp rax, 0\n";
            printf			"  je .Lbreak%d\n" context;
            (* execute *)
            emit_inner execution;
            (* post execute *)
            printf			".Lcontinue%d:\n" context;
            emit_inner iteration;
            (* goto begin *)
            printf			"  jmp .Lcondition%d\n" context;
            (* end *)
            printf			".Lbreak%d:\n" context
        | Node_While (condition, execution) ->
            let context = incr context; !context in
            (* begin: *)
            printf			".Lcontinue%d:\n" context;
            (* condition *)
            emit_inner condition;
            (* if 0, goto end *)
            print_string	"  pop rax\n";
            print_string	"  cmp rax, 0\n";
            printf			"  je  .Lbreak%d\n" context;
            (* execute & goto begin *)
            emit_inner execution;
            printf			"  jmp .Lcontinue%d\n" context;
            (* end: *)
            printf			".Lbreak%d:\n" context

        | Node_If (condition, if_true, if_false) ->
            let context = incr context; !context in
            emit_inner condition;
            print_string    "  pop rax\n";
            print_string    "  cmp rax, 0\n";
            printf		    "  je  .Lelse%d\n" context;
            emit_inner if_true;
            printf    		"  jmp .Lend%d\n" context;
            printf		    ".Lelse%d:\n" context;
            emit_inner if_false;
            printf    		".Lend%d:\n" context
        | Node_Return node -> 
            emit_inner node;
            print_string	"  jmp .Lreturn.main\n" (* TODO 関数名 *)
        | Node_Variable (size, _, _) as v ->
            emit_address v;
            load size
        | Node_Assign (size, left, right) ->
            emit_address left;
            emit_inner right;
            let prefix = size_prefix size in
			let register_name = register_name_rax size in
            print_string    "  pop rax\n";
            print_string    "  pop rdi\n";
            printf			"  mov %s [rdi], %s\n" prefix register_name;
            print_string    "  push rax\n"
        | Node_Int d ->
            printf  "  push %d\n" d;
            (* TODO returnの代替 *)
            printf  "  mov rax, %d\n" d
        | Node_Binary (_, op, left, right) ->
            emit_inner left;
            emit_inner right;
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
    in
    emit_inner node

let e globals = 
    print_string			".intel_syntax noprefix\n";
    print_string			".text\n";
    let rec emit_globals = function | [] -> () | global :: globals -> match global with
        | Function (name, params, body, stack) ->
			let rec adjust stack = if (stack mod 16) = 0 then stack else adjust (stack + 1) in
			let stack = adjust stack in
            printf   		".global %s\n" name;
            printf   		"%s:\n" name;
            (* プロローグ *)
            print_string	"  push rbp\n";
            print_string	"  mov rbp, rsp\n";
            printf          "  sub rsp, %d # stack size\n" stack;
            let stack_params i { Untyped.size; Untyped.offset; _} =
				let prefix = size_prefix size in
                let register = register_name_for_parameter i size in
                printf		"  lea rax, [rbp - %d]\n" offset;
                printf		"  mov %s [rax], %s\n" prefix register;
                ()
            in
            List.iteri stack_params params;
            List.iter emit body;
            printf   		".Lreturn.%s:\n" name;
            (* エピローグ *)
            print_string	"  mov rsp, rbp\n";
            print_string	"  pop rbp\n";
            print_string	"  ret\n";
            emit_globals globals
    in
    emit_globals globals