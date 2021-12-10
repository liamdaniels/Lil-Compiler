(* Handles conversion from AST to IR and outputting the IR *)

open Ast

type var = string

type tag = string

type irbinop = IRAdd | IRSub | IRLessThan | IREquals

type binop_order = BinLeft | BinRight | BinResult

type irval = IRVar of var | IRInt of int

type intermediate_aexp_tree =
  | IAValue of irval
  | IABinop of (irbinop * aexp * aexp)

type ir_com =
  | Const of (var * irval)
  | Print of irval
  | Tag of tag
  | Jump of tag
  | JumpZero of (var * tag)
  | Binop of irbinop * var * (irval * irval) 

let aexp_to_intermediate_arith = function
  | Plus (a1, a2) ->     IABinop (IRAdd, a1, a2)
  | Minus (a1, a2) ->    IABinop (IRSub, a1, a2)
  | LessThan (a1, a2) -> IABinop (IRLessThan, a1, a2)
  | Equals (a1, a2) ->   IABinop (IREquals, a1, a2)
  | Int i -> IAValue (IRInt i)
  | Var x -> IAValue (IRVar x)

let arith_var_name (order : binop_order) (depth : int) : string =
  let varname = match order with 
    | BinLeft -> "a"
    | BinRight -> "b"
    | BinResult -> "c"
  in
  "_" ^ varname ^ (string_of_int depth)

(** Precondition: [depth] >= 0 *)
let rec eval_a (depth : int) (a : aexp) : ir_com list = 
  let result_var = arith_var_name BinResult depth in
  let left_var = arith_var_name BinLeft depth in
  let right_var = arith_var_name BinRight depth in
  let prev_result = arith_var_name BinResult (depth + 1) in
  match aexp_to_intermediate_arith a with
  | IAValue value -> [Const (result_var, value)]
  | IABinop (binop, a1, a2) ->
      eval_a (depth + 1) a1 @
      [Const (left_var, IRVar prev_result)] @
      eval_a (depth + 1) a2 @
      [
        Const (right_var, IRVar prev_result);
        Binop (binop, result_var, (IRVar left_var, IRVar right_var))
      ]


let rec eval_com_r (num_tags : int ref) (c : com) : ir_com list =
  let arith_result_var = arith_var_name BinResult 0 in
  match c with
  | Seq (c1, c2) -> (eval_com_r num_tags c1) @ (eval_com_r num_tags c2)
  | Skip -> []
  | Assign (x, a) -> (eval_a 0 a) @ [Const (x, IRVar arith_result_var)]
  | Print a -> (eval_a 0 a) @ [Print (IRVar arith_result_var)]
  | If (a, c1, c2) -> eval_if a c1 c2 num_tags
  | While (a, c) -> eval_while a c num_tags

and eval_if a c1 c2 num_tags =
  let arith_result_var = arith_var_name BinResult 0 in
  let elsetag = "else" ^ string_of_int !num_tags in
  let exittag = "exitconditional" ^ string_of_int !num_tags in
  num_tags := !num_tags + 1; 
  (* The commands, in order, one per line: *)
  eval_a 0 a @
  [JumpZero (arith_result_var, elsetag)] @
  eval_com_r num_tags c1 @
  [Jump exittag;
   Tag elsetag] @
  eval_com_r num_tags c2 @
  [Tag exittag]

and eval_while a c num_tags =
  let arith_result_var = arith_var_name BinResult 0 in
  let starttag = "startwhile" ^ string_of_int !num_tags in
  let endtag = "endwhile" ^ string_of_int !num_tags in
  num_tags := !num_tags + 1;
  (* The commands, in order, one per line: *)
  [Tag starttag] @
  eval_a 0 a @
  [JumpZero (arith_result_var, endtag)] @
  eval_com_r num_tags c @
  [Jump starttag;
   Tag endtag]

let eval_com = let n = ref 0 in eval_com_r n

