(*
  main.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

open OLith

let compile source flags =
  Lexer.lex source