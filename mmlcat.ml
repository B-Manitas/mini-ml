(* Interprète MML *)

open Format
open Lexing

let usage = "usage: ./mmlcat file.mml"

let spec = []
  
let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".mml") then
        raise (Arg.Bad "no .mml extension");
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Mmlparser.program Mmllexer.token lb  in
    close_in c;
    let output_file = file ^ ".cat" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mmlpp.print_prog outf prog;
    close_out out;
    exit 0
  with
  | Mmllexer.Lexing_error s ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     eprintf "lexical error: %s@." s;
     exit 1
  | Mmlparser.Error ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     eprintf "syntax error@.";
     exit 1
  | e ->
     eprintf "Anomaly: %s\n@." (Printexc.to_string e);
     exit 2

