
open Printf

(* https://www.cs.cornell.edu/courses/cs3110/2018sp/l/08-functors/notes.html *)
module StringMap = Map.Make(String)
open StringMap

let emit_cmp op =
    print_string    "  cmp rax, rdi\n";
    printf          "  %s al\n" op;
    print_string    "  movzb rax, al\n"

let emit_address var_map = function
    | Node.Node_Variable name ->
        let offset = find name var_map in
        printf          "  # variable [%s]\n" name;
        printf          "  lea rax, [rbp - %d]\n" offset;
        print_string    "  push rax\n"
    | _ -> failwith "This node can't emit address."

let load _ =
    print_string    "  pop rax\n";
    (* 8byte == 64bit *)
    print_string    "  mov rax, [rax]\n"

let emit var_map nodes = 
    let emit_address = emit_address var_map in
    let rec emit_inner = function
        | Node.Node_Variable _ as v ->
            emit_address v;
            load v
        | Node.Node_Assign (left, right) ->
            emit_address left;
            emit_inner right;
            print_string    "  pop rax\n";
            print_string    "  pop rdi\n";
            print_string    "  mov QWORD PTR [rdi], rax\n";
            print_string    "  push rax\n"
        | Node.Node_Int d ->
            printf  "  push %d\n" d;
            (* TODO returnの代替 *)
            printf  "  mov rax, %d\n" d
        | Node.Node_Binary (op, left, right) ->
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
    emit_inner nodes

let rec calculate_stack_offset stack m = function
    | Node.Node_Int _ -> (stack, m)
    | Node.Node_Variable name ->
        begin
        match (find_opt name m) with
            | Some _ -> (stack, m)
            | None ->
                (* TODO 型ごとのサイズ *)
                let size = 8 in
                let offset = stack + size in
                (offset, add name offset m)
        end
    | Node.Node_Assign (left, right) -> 
        let (stack, m) = calculate_stack_offset stack m left in
        let (stack, m) = calculate_stack_offset stack m right in
        (stack, m)
    | Node.Node_Binary (_, left, right) ->
        let (stack, m) = calculate_stack_offset stack m left in
        let (stack, m) = calculate_stack_offset stack m right in
        (stack, m)

let rec stack_offset stack m = function
    | [] -> (stack, m)
    | node :: trees -> 
        let (stack, m) = calculate_stack_offset stack m node in
        stack_offset stack m trees

let e trees =
    let (stack, var_map) = stack_offset 0 empty trees in
    print_endline   ".intel_syntax noprefix";
    print_endline   ".text";
    print_endline   ".global main";
    print_endline   "main:";
    (* プロローグ *)
    print_endline   "  push rbp";
    print_endline   "  mov rbp, rsp";
    printf          "  sub rsp, %d\n" stack;
    List.iter (emit var_map) trees;
    print_endline   ".Lreturn.main:";
    (* エピローグ *)
    print_endline   "  mov rsp, rbp";
    print_endline   "  pop rbp";
    print_endline   "  ret";