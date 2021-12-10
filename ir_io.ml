open Ir

let string_of_val = function
  | IRInt i -> string_of_int i
  | IRVar x -> x

let output_ir_as_ir (oc : out_channel) = 
  let open Printf in 
  let string_of_binop = function
    | IRAdd -> "add"
    | IRSub -> "sub"
    | IRLessThan -> "lt"
    | IREquals -> "eq"
  in
  let space = "  " in
  let output_ir_line = function 
    | Const (x, v) -> fprintf oc "%sconst %s = %s\n" space x (string_of_val v)
    | Print v -> fprintf oc "%sprint %s\n" space (string_of_val v)
    | Binop (binop, x, (v1, v2)) ->
        fprintf oc "%s%s %s %s -> %s\n" space (string_of_binop binop)
          (string_of_val v1) (string_of_val v2) x
    | Tag t -> fprintf oc "%s:\n" t
    | Jump t -> fprintf oc "%sjump %s\n" space t
    | JumpZero (x, t) -> fprintf oc "%sjumpzero %s %s\n" space x t
  in
  List.iter output_ir_line

let output_ir_as_c (oc : out_channel) (insns : ir_com list) = 
  let open Printf in
  let string_of_bop = function
    | IRAdd -> "+"
    | IRSub -> "-"
    | IRLessThan -> "<"
    | IREquals -> "=="
  in
  let com_var = function
    | Const (x, _) -> Some x
    | Binop (_, x, _) -> Some x
    | _ -> None
  in
  let rec output_declarations used_vars = function
    | [] -> ()
    | com :: t ->
        begin
          match com_var com with
          | None -> output_declarations used_vars t
          | Some x -> 
              if List.mem x used_vars then output_declarations used_vars t
              else 
                begin 
                  fprintf oc "int %s;" x;
                  output_declarations (x :: used_vars) t
                end
        end
  in
  let rec output_ir_line = function
    | Const (x, v) -> fprintf oc "%s=%s;\n" x (string_of_val v)
    | Print v -> 
        fprintf oc "printf(\"%cd\\n\",%s);\n" '%' (string_of_val v)
    | Binop (binop, x, (v1, v2)) -> 
        fprintf oc "%s=%s%s%s;\n" x (string_of_val v1) 
          (string_of_bop binop) (string_of_val v2) 
    | Tag t -> fprintf oc "%s:\n" t
    | Jump t -> fprintf oc "goto %s;\n" t
    | JumpZero (x, t) -> fprintf oc "if(!%s) goto %s;\n" x t
  in
  fprintf oc "#include <stdio.h>\nint main(){\n";
  output_declarations [] insns;
  fprintf oc "\n";
  List.iter output_ir_line insns;
  fprintf oc "}\n"

