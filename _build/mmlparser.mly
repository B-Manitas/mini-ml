%{

  open Lexing
  open Mml

%}

%token PLUS STAR
%token <int> CST
%token <string> IDENT
%token EOF

%left PLUS
%left STAR

%start program
%type <Mml.prog> program

%%

program:
| (* à compléter *) code=expression EOF { {types=[]; code} }
;

simple_expression:
| n=CST { Int(n) }
;

expression:
| e=simple_expression { e }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
;

