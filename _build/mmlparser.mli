
(* The type of tokens. *)

type token = 
  | STAR
  | PLUS
  | IDENT of (string)
  | EOF
  | CST of (int)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog)
