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

type frame = sexpr list with_pos

let rec parse
  (tokens : (Lexer.pos_info * Lexer.token) list)
  (stck : frame list)
  (acc : ((sexpr, error) result) list)
  =
  let open Lexer in
  let ( &:= ) st en = {en with start = st.start} in
  match tokens with
  | [] -> (match stck with
            | [] -> List.rev acc
            | stck -> List.rev_append (List.map (fun v -> Error (UnclosedList v.p)) stck) acc 
          )
  | (pos, tok) :: rest -> (
    let push_atom atom =
      match stck with
      | [] -> parse rest stck (Ok (SAtom {p = pos; v = atom; }) :: acc)
      | top :: stck -> parse rest ({p = top.p &:= pos; v = (SAtom {p = pos; v = atom; }) :: top.v} :: stck) acc
    in
    let push_list init wrapper =
      parse rest ({p = pos; v = init} :: stck) acc
    in
    match tok with
    | TInt x -> push_atom (AInt x)
    | TFloat x -> push_atom (AFloat x)
    | TSymbol x -> push_atom (ASymbol x)
    | TString x -> push_atom (AString x)
    | TChar x -> push_atom (AChar x)
    | QUASIQUOTE -> push_atom (ASymbol "quasiquote")
    | UNQUOTE -> push_atom (ASymbol "unquote")
    | UNQUOTE_SPLICE -> push_atom (ASymbol "unquote_splice")
    | TLPAREN -> push_list [] false
    | TRPAREN -> (
      match stck with
      | [] -> parse rest [] (Error (UnmatchedRPAREN pos) :: acc)
      | h1 :: h2 :: stck -> parse rest ({p = h2.p &:= pos; v = SList {p = h1.p &:= pos; v = List.rev h1.v; } :: h2.v} :: stck) acc
      | h :: [] -> parse rest [] (Ok (SList {p = h.p &:= pos; v = List.rev h.v; }) :: acc)
    )
  )