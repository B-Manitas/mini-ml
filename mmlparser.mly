%{

  open Lexing
  open Mml

%}


(* Constante entière *)
%token <int> CST

(* Constante booléenne *)
%token <bool> BOOL

(* Symbole ??? *)
%token <string> IDENT

(* Symboles arithmétiques *)
%token PLUS STAR UNIT NEG PARENTHESIS_LEFT PARENTHESIS_RIGHT

(* Symboles variables *)
%token LET IN THEN ELSE


(* Autres symboles *)
%token NOT

(* Fin de fichier *)
%token EOF

(* Sens de l'association *)
%left PLUS
%left STAR

(* Début du programme *)
%start program
%type <Mml.prog> program

%%

program:
| (* à compléter *) code=expression EOF { {types=[]; code} }
// | error 
//   {
//     let pos = $startpos in
//     let message = Printf.printf 
//       "echec a la position %d %d"
//       pos.pos_lnum
//       (pos.pos_cnum - pos.pos_bol)
//     in
//     failwith message
//    }
;

simple_expression:
| n=CST { Int(n) }
| PARENTHESIS_LEFT e=expr PARENTHESIS_RIGHT { e }
| b=BOOL { Bool(b) }
| _=UNIT {}
;

expression:
| e=simple_expression { e }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e=expression op=unop { Uop(op, e) }
| Let e1=expression IN e2=expression { (* A COMPLETER *) }
| Let e1=expression THEN e2=expression { (* A COMPLETER *) }
| Let e1=expression THEN e2=expression ELSE e3=expression { (* A COMPLETER *) }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
;


%inline unop:
| NEG { Neg }
| NOT { Not }
;
