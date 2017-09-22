%{

  open SourceAst

%}

(* Définition des lexèmes et propriétés *)
%token <string> IDENT
%token BEGIN END
%token SEMI
%token INT
%token PRINT
%token EOF
%token MAIN
%token PLUS
%token MULT

(* Priotités pour résoudre le conflit shift/reduce *)
/*%nonassoc PLUS
%nonassoc MULT*/

%start main (* Non terminal  principal *)
%type <SourceAst.main> main

%% (* <- Séparation *)
(* Règles de reconnaissance *)

main: (* non terminal *)
| MAIN; BEGIN; INT; x=IDENT; END;
  BEGIN; vds=var_decls; is=instructions; END; EOF  {
    let infox = { typ=TypInteger; kind=FormalX } in
    let init  = Symb_Tbl.singleton x infox in
    let union_vars = fun _ _ v -> Some v in
    let locals = Symb_Tbl.union union_vars init vds in
    {locals = locals; code=is} }
;

var_decls:
| (* empty *)                             { Symb_Tbl.empty    }
(* À compléter *)
;

instructions:
| (* empty *)                             { []                }
| i=instruction; SEMI; is=instructions    { i :: is           }
;

instruction:
(* On traduit les règles de grammaire { instruction a faire } *)
| PRINT(*$0*); BEGIN(*$1*); e=expression(*$2*); END (*$3*)  { Print(e)   }
(*au lieu d'utiliser les $ varaibles comme en yacc ici on fait e =...*)
(* À compléter *)
;

expression:
| loc=location                            { Location(loc)     }
| e1 = expression; PLUS; e2 = expression { Binop(Add, e1, e2) }
| e1 = expression; MULT; e2 = expression { Binop(Mult, e1, e2) }
(* À compléter *)
;

location:
| id=IDENT  { Identifier id }
;
