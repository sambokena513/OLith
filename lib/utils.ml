(*
  utils.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

let validate source (tokens : (Lexer.pos_info * (Lexer.token, Lexer.error) result) list) =
  let display_lexer_error (pos, err) =
    let open Lexer in
    let print_err start =
      Printf.printf "%s at line %d, column %d, with start %d\n" start pos.line pos.column pos.start;
      Printf.printf "Source: %S\n" (String.sub source pos.start (pos.cursor - pos.start))
    in
    match err with
    | MalformedLexeme -> print_err "Malformed lexeme"
    | InvalidCharLit -> print_err "Invalid character literal"
    | InvalidStrLit -> print_err "Invalid string literal"
    | InvalidNumLit -> print_err "Invalid numeric literal"
    | UnclosedStrLit -> print_err "Unclosed string literal"
    | UnclosedCharLit -> print_err "Unclosed character literal"
    | UnclosedComment -> print_err "Unclosed comment"
  in let errors = 
    List.filter (fun (a, b) -> Result.is_error b) tokens
  in
  match errors with
  | [] -> Some (List.map (fun (a, b) -> (a, Result.get_ok b)) tokens)
  | xs -> List.iter (fun (a, b) -> display_lexer_error (a, Result.get_error b)) errors; None

let compile source flags =
  let lexed = Lexer.lex source in
  match validate source lexed with
  | Some x -> Ok (Parser.parse x [] [])
  | None -> Error "| Lexer Error |" 