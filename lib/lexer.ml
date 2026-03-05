(*
  lexer.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

type token =
  | TInt of int
  | TFloat of float
  | TSymbol of string
  | TString of string
  | TChar of char
  | QUASIQUOTE
  | UNQUOTE
  | UNQUOTE_SPLICE
  | TLPAREN
  | TRPAREN

type error =
  | MalformedLexeme
  | InvalidCharLit
  | InvalidStrLit
  | InvalidNumLit
  | UnclosedStrLit
  | UnclosedCharLit
  | UnclosedComment

type pos_info = {
  line : int;
  column : int;
  cursor : int;
  start : int;
}

let symbol_chars = Array.make 256 false
let () = String.iter (fun c -> symbol_chars.(Char.code c) <- true) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/<>=!?_&"

let num_chars = Array.make 256 false
let () = String.iter (fun c -> num_chars.(Char.code c) <- true) "0123456789.+-eE_"

let implode xs = String.of_seq (List.to_seq xs)

(**
  Tail-recursively lex a string containing Lisp source code, uses constant stack space.
  Returns a list of (pos_info * (token, error) result), or in other words, a list of tokens/errors, each bundled with its location.
  *)
let lex s = 
  let rec aux st acc =
    let len = String.length s in
    let advance state = { state with column = state.column + 1; cursor = state.cursor + 1 } in
    let advance_newline state = { state with line = state.line + 1; column = 0; cursor = state.cursor + 1 } in

    let new_lexeme state = { state with start = state.cursor } in

    let rec lex_comment st depth =
      if st.cursor < len && (s.[st.cursor] <> ']' || depth > 0) then
        match s.[st.cursor] with
        | '[' -> lex_comment (advance st) (depth + 1)
        | ']' -> lex_comment (advance st) (depth - 1)
        | '\n' -> lex_comment (advance_newline st) depth
        | _ -> lex_comment (advance st) depth
      else if st.cursor < len then
        ((advance st), false)
      else
        (st, true)
      in

    let rec lex_symbol st acc =
      if st.cursor < len && (symbol_chars.(Char.code s.[st.cursor]) || s.[st.cursor] = '[') then
        if s.[st.cursor] = '[' then
          match lex_comment (advance st) 0 with
          | (x, false) -> lex_symbol x acc
          | (x, true) -> (x, Error UnclosedComment)
        else
          lex_symbol (advance st) (s.[st.cursor] :: acc)
      else let lexeme = implode (List.rev acc) in
        (st, Ok (TSymbol lexeme))
      in

    let rec lex_string st acc escaped =
      if st.cursor < len then
        let c = s.[st.cursor] in
        match c, escaped with
        | '\n', _ -> lex_string (advance_newline st) (c :: acc) escaped
        | '\\', _ -> lex_string (advance st) (c :: acc) (not escaped)
        | '"', true -> lex_string (advance st) (c :: acc) false
        | '"', false -> (
                          try let str = Scanf.unescaped (implode (List.rev acc)) in
                            ((advance st), Ok (TString str))
                          with
                            Scanf.Scan_failure _ -> ((advance st), Error InvalidStrLit))
        | _, _ -> lex_string (advance st) (c :: acc) escaped
      else
        (st, Error UnclosedStrLit)
      in

    let rec lex_num st acc =
      if st.cursor < len && (num_chars.(Char.code s.[st.cursor]) || s.[st.cursor] = '[') then
        if s.[st.cursor] = '[' then
          match lex_comment (advance st) 0 with
          | (x, false) -> lex_num x acc
          | (x, true) -> (x, Error UnclosedComment)
        else
          lex_num (advance st) (s.[st.cursor] :: acc)
      else
        let lexeme = implode (List.rev acc) in
          match int_of_string_opt lexeme with
          | Some x -> 
            (st, Ok (TInt x))
          | None -> 
            match float_of_string_opt lexeme with
            | Some y -> (st, Ok (TFloat y))
            | None -> (st, Error InvalidNumLit)
          in

    let rec lex_char st acc =
      if st.cursor < len && s.[st.cursor] <> '\'' then
        lex_char (advance st) (s.[st.cursor] :: acc)
      else if st.cursor < len then
        let lexeme = (implode (List.rev acc)) in
        try
          let c = Scanf.unescaped lexeme in
            if String.length c <> 1 then ((advance st), Error InvalidCharLit)
            else ((advance st), Ok (TChar c.[0]))
        with
          Scanf.Scan_failure _ -> ((advance st), Error InvalidCharLit)
      else
        (st, Error UnclosedCharLit)
      in

    let rec step st =
      match s.[st.cursor] with
      | '(' -> (advance st, Ok TLPAREN)
      | ')' -> (advance st, Ok TRPAREN)
      | '`' -> (advance st, Ok QUASIQUOTE)
      | '@' -> (advance st, Ok UNQUOTE_SPLICE)
      | ',' -> (advance st, Ok UNQUOTE)
      | '"' -> lex_string (advance st) [] false
      | '\'' -> lex_char (advance st) []
      | '-' when st.cursor + 1 < len && Char.Ascii.is_digit s.[st.cursor + 1] -> lex_num st []
      | '-' -> lex_symbol st []
      | _ when Char.Ascii.is_digit s.[st.cursor] -> lex_num st []
      | _ when symbol_chars.(Char.code s.[st.cursor]) -> lex_symbol st []
      | _ -> ((advance st), Error MalformedLexeme)
    in if st.cursor >= len then List.rev acc
    else if s.[st.cursor] = '\n' then
      aux (advance_newline st) acc
    else if Char.Ascii.is_white s.[st.cursor] then
      aux (advance st) acc
    else if s.[st.cursor] = '[' then
        match lex_comment (advance (new_lexeme st)) 0 with
        | (x, false) -> aux x acc
        | (x, true) -> aux x ((x, Error UnclosedComment) :: acc)
    else
      let (new_state, tok) = step (new_lexeme st) in aux new_state ((new_state, tok) :: acc)
    in aux { line = 0; column = 0; cursor = 0; start = 0; } []