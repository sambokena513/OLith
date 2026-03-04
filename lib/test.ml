let refine r =
  match r with
  | Ok a -> a
  | Error _ -> failwith "Error value!"
  
let parse_unsafe s = Parser.parse (Lexer.lex s |> List.map (fun (a, b) -> (a, refine b))) [] [];;