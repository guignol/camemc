(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)


let () = 
    let input = Sys.argv.(1) in
    let reader index = try Some input.[index] with _ -> None in
    Emit.e 
        (Node.parse
            (Token.tokenize reader))
        
