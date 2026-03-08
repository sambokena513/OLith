(*
  parser.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

type 'a with_pos = { p : Lexer.pos_info; v : 'a  }

type atom =
  | AInt of int
  | AFloat of float
  | AString of string
  | ASymbol of string
  | AChar of char

type sexpr =
  | SAtom of atom with_pos
  | SList of sexpr list with_pos

type error =
  | UnclosedList of Lexer.pos_info
  | UnmatchedRPAREN of Lexer.pos_info
  | MissingTarget of Lexer.pos_info

type frame =
  | FList of sexpr list with_pos
  | FWrap of string with_pos

let rec parse
  (tokens : (Lexer.pos_info * Lexer.token) list)
  (stck : frame list)
  (acc : ((sexpr, error) result) list)
  =
  let open Lexer in
  let ( &= ) st en = {en with start = st.start} in
  let lift frame =
    match frame with
    | FList {p; _}
    | FWrap {p; _} -> Error (UnclosedList p)
  in
  match tokens with
  | [] -> (match stck with
            | [] -> List.rev acc
            | stck -> List.rev_append (List.map lift stck) acc 
          )
  | (pos, tok) :: rest -> (
    let rec push_expr expr stck =
      match stck with
      | [] -> parse rest stck (Ok expr :: acc)
      | FList {p; v} :: stck -> parse rest (FList {p = p &= pos; v = expr :: v} :: stck) acc
      | FWrap {p; v} :: stck -> push_expr (SList {p = p &= pos; v = [SAtom {p = pos; v = ASymbol v}; expr]}) stck
    in
    let push_atom atom = push_expr (SAtom {p = pos; v = atom}) stck in
    let push_frame init = parse rest (FList {p = pos; v = init} :: stck) acc in
    let push_frame_w init = parse rest (FWrap {p = pos; v = init} :: stck) acc in
    let pop_frame stck =
      match stck with
      | [] -> parse rest [] (Error (UnmatchedRPAREN pos) :: acc)
      | FList {p; v} :: stck -> push_expr (SList {p = p &= pos; v = List.rev v; }) stck
      | FWrap {p; _} :: stck -> parse rest stck (Error (MissingTarget p) :: acc)
    in
    match tok with
    | TInt x -> push_atom (AInt x)
    | TFloat x -> push_atom (AFloat x)
    | TSymbol x -> push_atom (ASymbol x)
    | TString x -> push_atom (AString x)
    | TChar x -> push_atom (AChar x)
    | QUASIQUOTE -> push_frame_w "quasiquote"
    | UNQUOTE -> push_frame_w "unquote"
    | UNQUOTE_SPLICE -> push_frame_w "unquote_splice"
    | TLPAREN -> push_frame []
    | TRPAREN -> pop_frame stck
  )