%{

  open Lexing
  open Mml

%}


(* Constante entière *)
%token <int> CST

(* Constante booléenne *)
%token <bool> BOOL

(* Symbole des noms des variables *)
%token <string> IDENT

(* Symbole des types *)
%token TINT TBOOL TUNIT

(* Symboles arithmétiques *)
%token PLUS MINUS STAR DIV PAR_L PAR_R MOD

(* Symboles variables *)
%token LET IN EQUAL

(* Symboles fonctions *)
%token IF THEN ELSE FUN REC

(* Symbol global *)
%token SEMICOLON ARROW_R COLON

(* Operation booléennes *)
%token TRUE FALSE NEQ DEQUAL LT GT LE GE AND OR NOT

(* Fin de fichier *)
%token EOF

(* Sens de l'association *)
%left SEMICOLON (* Faible prioritée *)
%left EQUAL
%left PLUS MINUS
%left STAR DIV MOD
%left PAR_L PAR_R CST INDENT (* Forte prioritée *)


(* Début du programme *)
%start program
%type <Mml.prog> program

%%

program:
| code=expression EOF 
  { 
    { types=[]; code } 
  }
;

simple_expression:
| n=CST { Int(n) }
| b=TRUE { Bool(true) }
| b=FALSE { Bool(false) }
| PAR_L PAR_R { Unit }
| x=IDENT { Var(x) }
| PAR_L e=expression PAR_R { e }
;

expression:
| e=simple_expression { e }
| f=expression e=simple_expression { App(f, e) }
| e1=expression SEMICOLON e2=expression { Seq(e1, e2) }
| op=unop e=expression { Uop(op, e) }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e1=expression op=invop e2=expression { Bop(op, e2, e1) }
| LET id=IDENT EQUAL e1=expression IN e2=expression { Let(id, e1, e2) }
| IF e1=expression THEN e2=expression { If(e1, e2, Unit) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1, e2, e3) }
| FUN PAR_L x=IDENT COLON typ=typ PAR_R ARROW_R e=expression { Fun(x, typ, e) }
| LET f=IDENT args=list(PAR_L x=IDENT COLON typ=typ PAR_R { (x, typ) }) EQUAL e1=expression IN e2=expression { Let(f, mk_fun args e1, e2) }
| LET REC f=IDENT args=list(PAR_L x=IDENT COLON t=typ PAR_R { (x, t) }) COLON typf=typ EQUAL e1=expression IN e2=expression 
{ Let(f, Fix(f, mk_fun_type args typf, mk_fun args e1), e2) }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| DIV { Div }
| DEQUAL { Eq }
| NEQ { Neq }
| AND { And }
| OR { Or }
| LT { Lt }
| LE { Le }
| MOD { Mod }
;

%inline invop:
| GT { Lt }
| GE { Le }
;

%inline unop:
| MINUS { Neg }
| NOT { Not }
;

%inline typ:
| TBOOL { TBool }
| TINT { TInt }
| TUNIT { TUnit }
;
