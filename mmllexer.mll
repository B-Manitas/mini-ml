{

  open Lexing
  open Mmlparser

  exception Lexing_error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter 
			(fun (s, k) -> Hashtbl.add h s k)
      [ 
        "let", LET;
        "in", IN;
        "if", IF;
        "then", THEN;
        "else", ELSE;
        "true", TRUE;
        "false", FALSE;
        (* "bool", BOOLEAN; *)
        (* "fun", FUN; *)
        (* "rec", REC; *)
        (* "mod", MOD; *)
        (* "not", NOT; *)
        (* "type", TYPE; *)
        (* "int", INTEGER; *)
        (* "unit", UNIT; *)
        (* "mutable", MUTABLE;   *)
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | ident as id 
    {keyword_or_ident id } 
  | "+"
      { PLUS }
  | "-" 
    { MINUS }
  | "*"
      { STAR }
  | "/"
    { DIV }
  | "=" 
      { EQUAL }
  | "==" 
      { DEQUAL }
  | "!=" 
      { NEQ }
  | "<" 
      { LT }
  | "<=" 
      { LE }
  | ">" 
      { GT }
  | ">=" 
      { GE }
  | "&&" 
      { AND }
  | "||" 
      { OR }
  | "(" 
      { PAR_L }
  | ")" 
      { PAR_R }
  (* | "{" 
      { BRACKET_L }
  | "}" 
      { BRACKET_R }
  | ";" 
      { SEMICOLON }
  | ":" 
      { COLON }
  | "." 
      { POINT }
  | "<-" 
      { ARROW_L }
  | "->" 
      { ARROW_R } *)
  | eof
      { EOF }
  | _
      { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { raise (Lexing_error "unterminated comment") }
