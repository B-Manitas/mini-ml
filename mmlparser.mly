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

(* Symboles arithmétiques *)
%token PLUS MINUS STAR DIV PAR_L PAR_R

(* Symboles variables *)
%token LET IN EQUAL

(* Symboles fonctions *)
%token IF THEN ELSE 

(* Operation booléennes *)
%token TRUE FALSE NEQ DEQUAL LT GT LE GE AND OR

(* Autres symboles *)
%token NOT

(* Fin de fichier *)
%token EOF

(* Sens de l'association *)
%left EQUAL (* Faible prioritée *)
%left PLUS MINUS
%left STAR DIV
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
| PAR_L PAR_R { Unit }
| n=CST { Int(n) }
| x=IDENT { Var(x) }
| PAR_L e=expression PAR_R { e }
| b=TRUE { Bool(true) }
| b=FALSE { Bool(false) }
;

expression:
| e=simple_expression { e }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e=expression op=unop { Uop(op, e) }
| LET ident=IDENT EQUAL e1=expression IN e2=expression { Let(ident, e1, e2) }
| IF e1=expression THEN e2=expression { If(e1, e2, Unit) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1, e2, e3) }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| DIV { Div }
| EQUAL { Eq }
| AND { And }
| OR { Or }
;

%inline unop:
| MINUS { Neg }
| NOT { Not }
;
