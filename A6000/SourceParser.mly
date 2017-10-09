%{

  open SourceAst

%}

(* Définition des lexèmes et propriétés *)
%token <string> IDENT
%token BEGIN END
%token SEMI
%token INT BOOL
%token <int> INTEGER
%token <bool> TRUE
%token <bool> FALSE
%token PRINT
%token EOF
%token MAIN
%token EQ
%token ADD
%token LT AND MULT NEQ LE OR SUB MINUS DIV INCR
(* ajouts  *)
%token VAR
%token IF THEN ELSE FOR
%token WHILE
%token SET


(* Priotités pour résoudre le conflit shift/reduce *)
%nonassoc LT AND LE OR
/*%nonassoc ADD SUB
%nonassoc MULT DIV*/
%left ADD SUB
%left MULT DIV

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
|                         { Symb_Tbl.empty }
| VAR; t = BOOL; ident = IDENT; SEMI; table = var_decls
  { let info = {typ=TypBoolean; kind=Local} in (Symb_Tbl.add ident info table) }
| VAR; t = INT; ident = IDENT; SEMI; table = var_decls
  { let info = {typ=TypInteger; kind=Local} in (Symb_Tbl.add ident info table) }

;

instructions:
| (* empty *)                             { []                }
| i=instruction; SEMI; is=instructions    { i :: is           }
;

instruction:
| loc = location; SET; e = expression { Set(loc, e) }
| PRINT; BEGIN; e=expression; END  { Print(e) }
| WHILE; e = expression; BEGIN; ins = instructions; END; { While(e, ins) }
| IF; e = expression; THEN; BEGIN; ins_if = instructions; END;
  ELSE; BEGIN; ins_else = instructions; END { If(e, ins_if, ins_else) }
| FOR; BEGIN; loc = location ; SEMI; lit = literal; END; BEGIN;
ins = instructions; END
  { let cond = Binop(Lt, Location(loc), Literal(lit) ) in
    let incr_i = Binop(Add, Location(loc), Literal(Int(1))) in
    let set = Set(loc, incr_i) in
    let new_ins = [set] @ ins in
    While(cond, new_ins)
  }
  | loc = location; INCR {
    let incr = Binop(Add, Location(loc), Literal(Int(1))) in
    Set(loc, incr)
  }


;

(*mettre un type : ici ? *)

expression:
| loc=location                            { Location(loc) }
| e1 = expression; ADD; e2 = expression   { Binop(Add, e1, e2) }
| e1 = expression; MULT; e2 = expression  { Binop(Mult, e1, e2) }
| e1 = expression; EQ; e2 = expression    { Binop(Eq, e1, e2) }
| e1 = expression; LT; e2 = expression    { Binop(Lt, e1, e2) }
| e1 = expression; LE; e2 = expression    { Binop(Le, e1, e2) }
| e1 = expression; OR; e2 = expression    { Binop(Or, e1, e2) }
| e1 = expression; AND; e2 = expression   { Binop(And, e1, e2) }
| e1 = expression; SUB; e2 = expression   { Binop(Sub, e1, e2) }
| e1 = expression; NEQ; e2 = expression   { Binop(Neq, e1, e2) }
| lit = literal                           { Literal(lit) }

;

literal:
| i=INTEGER { Int i }
| b = TRUE { Bool true }
| b = FALSE { Bool false }
;

location:
| id=IDENT  { Identifier id }
;
