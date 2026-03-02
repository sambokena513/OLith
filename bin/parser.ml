type atom =
  | AInt of int
  | AFloat of float
  | AString of string
  | ASymbol of string
  | AChar of char

type sexpr =
  | SAtom of atom
  | SList of sexpr list

type error = UnclosedList

let parse (tokens : (Lexer.pos_info * Lexer.token) list) (stck : (Lexer.pos_info * sexpr list) list) (acc : (sexpr, error) result list) =
  failwith "aaahhh" (* tf did I write?! *)