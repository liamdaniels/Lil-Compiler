(* AST for a language that's even smaller than IMP *)

type var = string

(* Arithmetic expressions (which function as booleans, too) *)
type aexp = 
  | Int of int
  | Var of var
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | LessThan of aexp * aexp
  | Equals of aexp * aexp

(* Commands *)
type com =
  | Skip
  | Assign of var * aexp
  | Seq of com * com
  | If of aexp * com * com
  | While of aexp * com
  | Print of aexp

