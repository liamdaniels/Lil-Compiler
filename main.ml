(* Main *)

let ast_of_file filename = 
  let lexbuf = Lexing.from_channel (open_in filename) in
  try Parser.parse_program Lexer.token lexbuf
  with Parsing.Parse_error ->
    Printf.printf "Syntax error at line %d\n" !Lexer.line_number; 
    exit 1 

let () =
  let _ =
    if Array.length Sys.argv < 2 then
      (Printf.printf "Please supply file\n";
       exit 0);
    if Array.length Sys.argv > 3 then 
      (Printf.printf "Please use fewer arguments\n";
       exit 0)
  in
  let output_func = 
    if Array.length Sys.argv == 3 then 
      match Sys.argv.(2) with
      | "-C" -> Ir_io.output_ir_as_c
      | _ -> Printf.printf "Unknown argument\n"; exit 0
    else Ir_io.output_ir_as_ir
  in
  let filename = Sys.argv.(1) in
  let tree = ast_of_file filename in
  tree |> Ir.eval_com |> output_func stdout
