
open Printf

let context = ref 0

let registers_64 = ["rdi";
                    "rsi";
                    "rdx";
                    "rcx";
                    "r8";
                    "r9"]

let emit_cmp op =
    print_string    "  cmp rax, rdi\n";
    printf          "  %s al\n" op;
    print_string    "  movzb rax, al\n"

let load _ =
    print_string    "  pop rax\n";
    (* 8byte == 64bit *)
    print_string    "  mov rax, [rax]\n";
    print_string    "  push rax\n"

let emit node = 
    let rec emit_address = function
        | Parser.Node_Variable { variable = { name; _ }; offset; } ->
            printf          "  # variable [%s]\n" name;
            printf          "  lea rax, [rbp - %d]\n" offset;
            print_string    "  push rax\n"
        | Parser.Node_Deref node -> emit_inner node
        | _ -> failwith "This node can't emit address."
    and emit_inner = function
        | Parser.Node_Address node -> 
            (* 変数のアドレスをスタックに積むだけ *)
            emit_address node
        | Parser.Node_Deref node as deref ->
            (* ポインタ変数の値（アドレス）をスタックに積む *)
            emit_inner node;
            (* それをロードする *)
            load deref
        | Parser.Node_Call (name, args) -> 
            List.iter emit_inner args;
            let count = List.length args in
            let pop i _ = 
                let register = List.nth registers_64 (count - i - 1) in
                printf		" pop %s\n" register
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
        | Parser.Node_No_Op -> 
            print_string	"  # no op\n"
        | Parser.Node_Block nodes ->
            List.iter emit_inner nodes
        | Parser.Node_For (init, condition, iteration, execution) ->
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
        | Parser.Node_While (condition, execution) ->
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

        | Parser.Node_If (condition, if_true, if_false) ->
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
        | Parser.Node_Return node -> 
            emit_inner node;
            print_string	"  jmp .Lreturn.main\n"
        | Parser.Node_Variable _ as v ->
            emit_address v;
            load v
        | Parser.Node_Assign (left, right) ->
            emit_address left;
            emit_inner right;
            print_string    "  pop rax\n";
            print_string    "  pop rdi\n";
            print_string    "  mov QWORD PTR [rdi], rax\n";
            print_string    "  push rax\n"
        | Parser.Node_Int d ->
            printf  "  push %d\n" d;
            (* TODO returnの代替 *)
            printf  "  mov rax, %d\n" d
        | Parser.Node_Binary (op, left, right) ->
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
        | Parser.Function ({ name; _ }, params, body, stack) ->
            printf   		".global %s\n" name;
            printf   		"%s:\n" name;
            (* プロローグ *)
            print_string	"  push rbp\n";
            print_string	"  mov rbp, rsp\n";
            printf          "  sub rsp, %d # stack size\n" stack;
            let stack_params i decl = 
                let offset = decl.Parser.offset in
                let register = List.nth registers_64 i in
                printf		"  lea rax, [rbp - %d]\n" offset;
                printf		"  mov QWORD PTR [rax], %s\n" register;
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