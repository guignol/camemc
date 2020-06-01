
open Printf

(* https://www.cs.cornell.edu/courses/cs3110/2018sp/l/08-functors/notes.html *)
module StringMap = Map.Make(String)
open StringMap

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

let emit_address var_map = function
    | Ast.Node_Variable name ->
        let offset = find name var_map in
        printf          "  # variable [%s]\n" name;
        printf          "  lea rax, [rbp - %d]\n" offset;
        print_string    "  push rax\n"
    | _ -> failwith "This node can't emit address."

let load _ =
    print_string    "  pop rax\n";
    (* 8byte == 64bit *)
    print_string    "  mov rax, [rax]\n";
    print_string    "  push rax\n"

let emit var_map node = 
    let emit_address = emit_address var_map in
    let rec emit_inner = function
        | Ast.Node_Call (name, args) -> 
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
            let context = 444 in
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
        | Ast.Node_No_Op -> 
            print_string	"  # no op\n"
        | Ast.Node_Block nodes ->
            List.iter emit_inner nodes
        | Ast.Node_For (init, condition, iteration, execution) ->
            let context = 333 in
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
        | Ast.Node_While (condition, execution) ->
            let context = 222 in
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

        | Ast.Node_If (condition, if_true, if_false) ->
            let context = 111 in
            emit_inner condition;
            print_string    "  pop rax\n";
            print_string    "  cmp rax, 0\n";
            printf		    "  je  .Lelse%d\n" context;
            emit_inner if_true;
            printf    		"  jmp .Lend%d\n" context;
            printf		    ".Lelse%d:\n" context;
            emit_inner if_false;
            printf    		".Lend%d:\n" context
        | Ast.Node_Return node -> 
            emit_inner node;
            print_string	"  jmp .Lreturn.main\n"
        | Ast.Node_Variable _ as v ->
            emit_address v;
            load v
        | Ast.Node_Assign (left, right) ->
            emit_address left;
            emit_inner right;
            print_string    "  pop rax\n";
            print_string    "  pop rdi\n";
            print_string    "  mov QWORD PTR [rdi], rax\n";
            print_string    "  push rax\n"
        | Ast.Node_Int d ->
            printf  "  push %d\n" d;
            (* TODO returnの代替 *)
            printf  "  mov rax, %d\n" d
        | Ast.Node_Binary (op, left, right) ->
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

let rec aggregate variables nodes = 
    let rec from_one_node variables = function
        | Ast.Node_Variable name -> name :: variables
        | Ast.Node_No_Op -> variables
        | Ast.Node_Call (_, nodes) -> aggregate variables nodes
        | Ast.Node_Block nodes -> aggregate variables nodes
        | Ast.Node_For (init, condition, iteration, execution) ->
            aggregate variables [init; condition; iteration; execution]
        | Ast.Node_While (condition, execution) ->
            aggregate variables [condition; execution]
        | Ast.Node_If (condition, if_true, if_false) ->
            aggregate variables [condition; if_true; if_false]
        | Ast.Node_Return node -> from_one_node variables node
        | Ast.Node_Int _ -> variables
        | Ast.Node_Assign (left, right) -> aggregate variables [left; right]
        | Ast.Node_Binary (_, left, right) -> aggregate variables [left; right]
    in
    match nodes with | [] -> variables
                     | head :: tail -> 
                         aggregate (from_one_node variables head) tail

let rec calculate_stack_offset (stack, m) = function
    | [] -> (stack, m)
    | name :: tail ->
        calculate_stack_offset
            begin
                match (find_opt name m) with
                | Some _ -> (stack, m)
                | None ->
                    (* TODO 型ごとのサイズ *)
                    let size = 8 in
                    let offset = stack + size in
                    (offset, add name offset m)
            end
            tail

let e globals = 
    print_string   ".intel_syntax noprefix\n";
    print_string   ".text\n";
    let rec emit_globals = function | [] -> () | global :: globals -> match global with
        | Ast.Function (name, args, nodes) ->
            printf   		".global %s\n" name;
            printf   		"%s:\n" name;
            let variables = aggregate [] nodes in
            let (stack, var_map) = calculate_stack_offset (0, empty) (args @ variables) in
            (* プロローグ *)
            print_string	"  push rbp\n";
            print_string	"  mov rbp, rsp\n";
            printf          "  sub rsp, %d # stack size\n" stack;
            let stack_arg i name = 
                let offset = find name var_map in
                let register = List.nth registers_64 i in
                printf	"  lea rax, [rbp - %d]\n" offset;
                printf	"  mov QWORD PTR [rax], %s\n" register;
                ()
            in
            List.iteri stack_arg args;
            List.iter (emit var_map) nodes;
            printf   		".Lreturn.%s:\n" name;
            (* エピローグ *)
            print_string	"  mov rsp, rbp\n";
            print_string	"  pop rbp\n";
            print_string	"  ret\n";
            emit_globals globals
    in
    emit_globals globals