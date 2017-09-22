
(* The type of tokens. *)

type token = 
  | SEMI
  | PRINT
  | PLUS
  | MULT
  | MAIN
  | INT
  | IDENT of (string)
  | EOF
  | END
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.main)
