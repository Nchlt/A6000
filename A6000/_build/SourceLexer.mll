{

  open Lexing
  open SourceParser
  (* Ref au nombre de ligne *)
  let line_nb = ref 1;

  exception Erreur_syntaxique of string

  let erreur_syntaxique msg =
    raise ( Erreur_syntaxique ( msg ^ " ligne : " ^ ( string_of_int !line_nb )))

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	"integer",  INT;
  "print",    PRINT;
	"main",     MAIN;
  "var",      VAR;
  "boolean",  BOOL;
  "if",       IF;
  "then",     THEN;
  "else",     ELSE;
  "while",    WHILE;
  "true",     TRUE(true);
  "false",    FALSE(false);
  "for",      FOR
  (* Ajout *)
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  (*Si on voit un saut de ligne on incrémente line_nb et on lit la suite :*)
  | '\n' {incr line_nb; token lexbuf} (*Pour l'extension msg d'erreurs*)
  | [' ' '\t' '\r']+
      { token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "("
      { BEGIN }
  | ")"
      { END }
  | ";"
      { SEMI }
  | "var"
      { VAR }
  | "+"
    { ADD }
  | "-"
    { SUB }
  | "*"
    { MULT }
  | "/"
    { DIV }
  | ":=" { SET } (* ADD EQ LT AND MULT NEQ LE OR SUB *)
  | "=" { EQ }
  | "<" { LT }
  | "&&" { AND }
  | "!=" { NEQ }
  | "<=" { LE }
  | "||" { OR }
  | (digit)+ { INTEGER (int_of_string (lexeme lexbuf))}
  | "true" { TRUE(true) }
  | "false" { FALSE(false) }
  | "++" { INCR }
  | _ {failwith (erreur_syntaxique "Caratère non reconu")}
  | eof
      { EOF }

and comment = parse
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "Unterminated comment" }
