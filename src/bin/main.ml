(* MEMO *)
(* https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml *)
(* https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)


let () =
    let input = Sys.argv.(1) in
    let reader = if input = "-f"
        then
            (* ファイル名から読み込む *)
            let input = Sys.argv.(2) in
            let channel = open_in input in
            (fun index -> 
                 let () = seek_in channel index in
                 try Some (input_char channel) with End_of_file -> None
            )
        else
            (* 引数から読み込む *)
            (fun index -> 
                 try Some input.[index] with _ -> None
            )
    in
    Lexer.tokenize reader
    |> Parser.parse
    |> Typed.typed
    |> Untyped.untyped
    |> Emit.e;
    (* let _ = reader 1 in *)

