{
open Parser
open Printf
exception LexingError of string

let line_number = ref 1

let newline lexbuf = incr line_number

let error lexbuf = 
  let err = Printf.sprintf "error on line %d" !line_number in
  raise (LexingError err)
}

let digit = ['-']?['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9' '_']*

rule token = parse
| ' '                     { token lexbuf }
| '\n'                    { newline lexbuf; token lexbuf }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '<'                     { LESSTHAN }
| '='                     { EQUALS }
| ','                     { COMMA }
| ';'                     { SEMICOLON }
| '('                     { LPAREN }
| ')'                     { RPAREN }
| ":="                    { ASSIGN }
| "if"                    { IF }
| "fi"                    { FI }
| "else"                  { ELSE }
| "while"                 { WHILE }
| "elihw"                 { ELIHW }
| "done"                  { SKIP }
| "print"                 { PRINT }
| id as v                 { VAR(v) }
| digit+ as n             { INT(int_of_string n) }
| eof                     { EOF }
| _                       { error lexbuf }


