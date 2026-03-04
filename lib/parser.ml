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
  let finalize_pos st en = {en with start = st.start} in
  match tokens with
  | [] -> List.rev acc
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
      | TInt x -> parse rest ((ps, (SAtom (AInt x)) :: top) :: stck) acc
      | TFloat x -> parse rest ((ps, (SAtom (AFloat x)) :: top) :: stck) acc
      | TSymbol x -> parse rest ((ps, (SAtom (ASymbol x)) :: top) :: stck) acc
      | TString x -> parse rest ((ps, (SAtom (AString x)) :: top) :: stck) acc
      | TChar x -> parse rest ((ps, (SAtom (AChar x)) :: top) :: stck) acc
      | QUASIQUOTE -> parse rest ((ps, (SAtom (ASymbol "quasiquote")) :: top) :: stck) acc
      | UNQUOTE -> parse rest ((ps, (SAtom (ASymbol "unquote")) :: top) :: stck) acc
      | UNQUOTE_SPLICE -> parse rest ((ps, (SAtom (ASymbol "unquote_splice")) :: top) :: stck) acc
      | TLPAREN -> parse rest ((ps, []) :: stck) acc
      | TRPAREN -> parse rest stck ((finalize_pos ps pos, Ok (SList (List.rev top))) :: acc)
    )
  )