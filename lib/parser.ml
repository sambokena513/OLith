(*
  parser.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

type atom =
  | AInt of int
  | AFloat of float
  | AString of string
  | ASymbol of string
  | AChar of char

type sexpr =
  | SAtom of Lexer.pos_info * atom
  | SList of Lexer.pos_info * sexpr list

type error =
  | UnclosedList of Lexer.pos_info
  | UnmatchedRPAREN of Lexer.pos_info

let rec parse
  (tokens : (Lexer.pos_info * Lexer.token) list)
  (stck : (Lexer.pos_info * sexpr list) list)
  (acc : ((sexpr, error) result) list)
  =
  let open Lexer in
  let update_pos st en = {en with start = st.start} in
  match tokens with
  | [] -> (match stck with
            | [] -> List.rev acc
            | stck -> List.rev_append (List.map (fun (p, _) -> Error (UnclosedList p)) stck) acc 
          )
  | (pos, tok) :: rest -> (
    let push_atom atom =
      match stck with
      | [] -> parse rest stck (Ok (SAtom (pos, atom)) :: acc)
      | (ps, top) :: stck -> parse rest (((update_pos ps pos), (SAtom (pos, atom)) :: top) :: stck) acc
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
    | TLPAREN -> parse rest ((pos, []) :: stck) acc
    | TRPAREN -> (
      match stck with
      | [] -> parse rest [] (Error (UnmatchedRPAREN pos) :: acc)
      | (ps, top) :: (pps, ptop) :: stck -> parse rest ((update_pos pps pos, SList (update_pos ps pos, List.rev top) :: ptop) :: stck) acc
      | (ps, top) :: [] -> parse rest [] (Ok (SList (update_pos ps pos, List.rev top)) :: acc)
    )
  )