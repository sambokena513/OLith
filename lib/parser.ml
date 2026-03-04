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
  | SAtom of atom
  | SList of sexpr list

type error =
  | UnclosedList
  | UnmatchedRPAREN

let rec parse
  (tokens : (Lexer.pos_info * Lexer.token) list)
  (stck : (Lexer.pos_info * sexpr list) list)
  (acc : (Lexer.pos_info * (sexpr, error) result) list)
  =
  let open Lexer in
  let update_pos st en = {en with start = st.start} in
  let lift_st (a, _) = (a, Error UnclosedList) in
  match tokens with
  | [] -> (match stck with
            | [] -> List.rev acc
            | stck -> List.rev_append (List.map lift_st stck) acc 
          )
  | (pos, tok) :: rest -> (
    match stck with
    | [] -> (
      match tok with
      | TInt x -> parse rest stck ((pos, Ok (SAtom (AInt x))) :: acc)
      | TFloat x -> parse rest stck ((pos, Ok (SAtom (AFloat x))) :: acc)
      | TSymbol x -> parse rest stck ((pos, Ok (SAtom (ASymbol x))) :: acc)
      | TString x -> parse rest stck ((pos, Ok (SAtom (AString x))) :: acc)
      | TChar x -> parse rest stck ((pos, Ok (SAtom (AChar x))) :: acc)
      | QUASIQUOTE -> parse rest stck ((pos, Ok (SAtom (ASymbol "quasiquote"))) :: acc)
      | UNQUOTE -> parse rest stck ((pos, Ok (SAtom (ASymbol "unquote"))) :: acc)
      | UNQUOTE_SPLICE -> parse rest stck ((pos, Ok (SAtom (ASymbol "unquote_splice"))) :: acc)
      | TLPAREN -> parse rest ((pos, []) :: stck) acc
      | TRPAREN -> parse rest stck ((pos, Error UnmatchedRPAREN) :: acc)
    )
    | (ps, top) :: stck -> (
      match tok with
      | TInt x -> parse rest (((update_pos ps pos), (SAtom (AInt x)) :: top) :: stck) acc
      | TFloat x -> parse rest (((update_pos ps pos), (SAtom (AFloat x)) :: top) :: stck) acc
      | TSymbol x -> parse rest (((update_pos ps pos), (SAtom (ASymbol x)) :: top) :: stck) acc
      | TString x -> parse rest (((update_pos ps pos), (SAtom (AString x)) :: top) :: stck) acc
      | TChar x -> parse rest (((update_pos ps pos), (SAtom (AChar x)) :: top) :: stck) acc
      | QUASIQUOTE -> parse rest (((update_pos ps pos), (SAtom (ASymbol "quasiquote")) :: top) :: stck) acc
      | UNQUOTE -> parse rest (((update_pos ps pos), (SAtom (ASymbol "unquote")) :: top) :: stck) acc
      | UNQUOTE_SPLICE -> parse rest (((update_pos ps pos), (SAtom (ASymbol "unquote_splice")) :: top) :: stck) acc
      | TLPAREN -> parse rest (((update_pos ps pos), []) :: stck) acc
      | TRPAREN -> parse rest stck ((update_pos ps pos, Ok (SList (List.rev top))) :: acc)
    )
  )