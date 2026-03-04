(*
  test.ml

  Copyright (C) 2026 Samuel Peciar
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or 
  any later version.
*)

let refine r =
  match r with
  | Ok a -> a
  | Error _ -> failwith "Error value!"
  
let parse_unsafe s = Parser.parse (Lexer.lex s |> List.map (fun (a, b) -> (a, refine b))) [] [];;