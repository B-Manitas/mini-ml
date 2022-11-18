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
%token PLUS MINUS STAR PAR_L PAR_R

(* Symboles variables *)
%token LET IN THEN ELSE EQUAL

(* Autres symboles *)
%token NOT

(* Fin de fichier *)
%token EOF

(* Sens de l'association *)
// %left EQUAL (* Faible prioritée *)
%left PLUS MINUS
%left STAR 
%left PAR_L PAR_R CST INDENT (* Forte prioritée *)


(* Début du programme *)
%start program
%type <Mml.prog> program

%%

program:
| code=expression EOF 
  { 
    Printf.printf "OK";
    { types=[]; code } 
  }
;

simple_expression:
| n=CST { Int(n) }
| PAR_L e=expression PAR_R { e }
| b=BOOL { Bool(b) }
;

expression:
| e=simple_expression { e }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e=expression op=unop { Uop(op, e) }
(*| LET e1=expression IN e2=expression { (* A COMPLETER *) }*)
(*| LET e1=expression THEN e2=expression { (* A COMPLETER *) }*)
(*| LET e1=expression THEN e2=expression ELSE e3=expression { (* A COMPLETER *) }*)
;

%inline binop:
| EQUAL { Eq }
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
;



%inline unop:
| MINUS { Neg }
| NOT { Not }
;
