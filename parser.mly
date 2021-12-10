%{
open Ast
open Printf
%}

%token <string> VAR
%token <int> INT
%token PLUS MINUS LESSTHAN EQUALS 
       SKIP ASSIGN PRINT
       IF ELSE WHILE FI ELIHW
       LPAREN RPAREN COMMA SEMICOLON
%token EOF

%type <Ast.aexp> a
%type <Ast.com> c
%type <Ast.com> parse_program

%start parse_program

%%

a : a PLUS aa                                           { Plus($1, $3) }
  | a MINUS aa                                          { Minus($1, $3) }
  | aa                                                  { $1 }

aa : INT                                                { Int($1) }
   | VAR                                                { Var($1) }
   | LPAREN a RPAREN                                    { $2 }

c : ic SEMICOLON c                                      { Seq($1, $3) }
  | ic                                                  { $1 }

ic : IF a COMMA c ELSE c FI                             { If($2, $4, $6) }
   | WHILE a COMMA c ELIHW                              { While($2, $4) }
   | ac                                                 { $1 }

ac : SKIP                                               { Skip }
   | VAR ASSIGN a                                       { Assign($1, $3) }
   | PRINT a                                            { Print($2) }

parse_program : c EOF                                   { $1 }

