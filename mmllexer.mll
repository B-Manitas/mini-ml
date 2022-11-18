{

  open Lexing
  open Mmlparser

  exception Lexing_error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true": TRUE,
        "false": FALSE,
        "fun": FUN,
        "let": LET,
        "rec": REC,
        "in": IN,
        "if": IF,
        "then": THEN,
        "else": ELSE,
        "mod": MOD,
        "not": NOT,
        "type": TYPE,
        "int": INT,
        "bool": BOOL,
        "unit": UNIT,
        "mutable": MUTABLE
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
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "=" 
      { EQUAL }
  | "==" 
      { DOUBLE_EQUAL }
  | "!=" 
      { DIFFERENT }
  | "<" 
      { LESS }
  | "<=" 
      { LESS_EQUAL }
  | ">" 
      { GREATER }
  | ">=" 
      { GREATER_EQUAL }
  | "&&" 
      { AND }
  | "||" 
      { OR }
  | "(" 
      { PARENTHESIS_LEFT }
  | ")" 
      { PARENTHESIS_RIGHT }
  | "{" 
      { BRACKET_LEFT }
  | "}" 
      { BRACKET_RIGHT }
  | ";" 
      { SEMICOLON }
  | ":" 
      { COLON }
  | "." 
      { POINT }
  | "<-" 
      { LEFT_ARROW }
  | "->" 
      { RIGHT_ARROW }
  | "()" 
      { PARENTHESIS  }
  | "-" 
    { NEG }
  | _
      { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }
  | eof
      { EOF }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { raise (Lexing_error "unterminated comment") }
