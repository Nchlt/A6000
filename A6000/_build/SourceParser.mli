
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TRUE of (bool)
  | THEN
  | SUB
  | SET
  | SEMI
  | PRINT
  | OR
  | NEQ
  | MULT
  | MINUS
  | MAIN
  | LT
  | LE
  | INTEGER of (int)
  | INT
  | INCR
  | IF
  | IDENT of (string)
  | FOR
  | FALSE of (bool)
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | BOOL
  | BEGIN
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.main)
