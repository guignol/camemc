
open Printf
open String

type token =
    | Number of int
    | Reserved of string
    | Identifier of string
    | String of string

let starts_with str ch = match index_opt str ch with
    | Some index -> index = 0
    | None -> false

let degit_of_char = function
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_alnum ch = is_alpha ch || (degit_of_char ch) <> None
(* cf. string.ml *)
let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

let tokenize reader =
    let rec read_input cursor tokens =
        let buffer ch length tail_check =
            let rec get count buffer =
                if length = count then
                    match reader (cursor + count) with
                    | None -> Some buffer (* 末尾 *)
                    | Some ch -> if tail_check ch then Some buffer else None
                else match reader (cursor + count) with
                    | None -> None
                    | Some ch -> get (count + 1) (buffer ^ String.make 1 ch)
            in
            get 1 (String.make 1 ch)
        in
        let equals ch tail_check target =
            let length = String.length target in
            match buffer ch length tail_check with
            | Some str when str = target -> Some (length, target)
            | _ -> None
        in
        let read_reserved offset r		= read_input (cursor + offset) (tokens @ [Reserved r]) in
        let read_number offset d		= read_input (cursor + offset) (tokens @ [Number d]) in
        let read_identifier offset id	= read_input (cursor + offset) (tokens @ [Identifier id]) in
        let read_string offset literal	= read_input (cursor + offset) (tokens @ [String literal]) in
        let search_keywords ch = List.find_map (equals ch (fun ch -> not (is_alnum ch))) in
        let search_symbols ch = List.find_map (equals ch (fun _ -> true)) in
        let skip_comments ch = 
            match equals ch (fun _ -> true) "//" with
            | None -> None 
            | Some (_, _) -> 
                let rec search_line_break count =
                    match reader (cursor + count) with
                    | None -> count (* 末尾 *)
                    | Some ch -> if ch = '\n' then count + 1 else search_line_break (count + 1)
                in
                Some (search_line_break 2)
        in
        let read_string_literal ch =
            match equals ch (fun _ -> true) "\"" with
            | Some (_, _) -> 
                let rec concat_string count buffer =
                    match reader (cursor + count) with
                    | None -> failwith "" (* 末尾 *)
                    | Some ch -> 
                        if ch = '\"' 
                        then count + 1, buffer
                        else if ch = '\\' (* エスケープ *)
                        then 
                            let next = Option.get (reader (cursor + count + 1)) in
                            concat_string (count + 2) (buffer ^ String.make 1 ch ^ String.make 1 next)
                        else concat_string (count + 1) (buffer ^ String.make 1 ch)
                in
                Some (concat_string 1 "")
            | None -> None 
        in
        match reader cursor with None -> tokens | Some ch ->
            (* スペース *)
            if is_space ch then read_input (cursor + 1) tokens else
            match skip_comments ch with Some offset -> read_input (cursor + offset) tokens | None ->
            match read_string_literal ch with Some (offset, literal) -> read_string offset literal | None ->
            (* Keyword *)
            match search_keywords ch ["return"; "if"; "else"; "while"; "for"; "int"; "char"; "sizeof"] with 
            | Some (offset, found) -> read_reserved offset found | None ->
                (* 記号 *)
                (* 2文字 *)
                match search_symbols ch ["=="; "!="; "<="; ">=";] with 
                | Some (_, found) -> read_reserved 2 found | None ->
                    (* 1文字 *)
                    match search_symbols ch ["="; ";"; "+"; "-"; "*"; "&"; "/"; "("; ")"; "<"; ">"; "{"; "}"; ","; "["; "]"] with 
                    | Some (_, found) -> read_reserved 1 found | None ->
                        (* 数値 *)
                        let rec concat_number offset number = match reader (cursor + offset) with
                            | None -> (offset, number)
                            | Some ch -> match degit_of_char ch with
                                | None -> (offset, number)
                                | Some n -> concat_number (offset + 1) (number * 10 + n)
                        in
                        let (offset, number) = concat_number 0 0 in
                        if 0 < offset then read_number offset number else
                        (* その他識別子 *)
                        let rec concat_identifier count buffer = match reader (cursor + count) with
                            | None -> None
                            | Some ch ->
                                if is_alnum ch
                                then concat_identifier (count + 1) (buffer ^ String.make 1 ch)
                                else Some (count, buffer)
                        in
                        if is_alpha ch
                        then match concat_identifier 1 (String.make 1 ch) with
                            | Some (offset, id) -> read_identifier offset id
                            | None -> failwith ""
                        else failwith (sprintf "cannot tokenize %c" ch)
    in
    read_input 0 []

let debug_string_of_token = function
    | Number d -> sprintf "%d (number)" d
    | Reserved r -> sprintf "%s (reserved)" r
    | Identifier s -> sprintf "%s (identifier)" s
    | String s -> sprintf "\"%s\" (string literal)" s

let print_token t = print_string (debug_string_of_token t)

(*let () = List.iter debug_print_token (read 1)*)