(* Interprète Mini-ML *)

open Mml

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make(String)

(* Valeurs *)
type value =
  | VInt   of int
  | VBool  of bool
  | VUnit
  | VPtr   of int
(* Élements du tas *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t

let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Interprétation d'un programme complet *)
let eval_prog (p: prog): value =
  
  (* Initialisation de la mémoire globale *)
  let (mem: (value, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval (e: expr) (env: value Env.t): value = 
    match e with
    | Int n  -> VInt n
    | Bool b -> VBool b
    | Unit -> VUnit
    | Uop(Neg, e) -> VInt (-(evali e env))
    | Uop(Not, e) -> VBool (not (evalb e env))
    | Bop(Sub, e1, e2) -> VInt (evali e1 env - evali e2 env)
    | Bop(Add, e1, e2) -> VInt (evali e1 env + evali e2 env)
    | Bop(Mul, e1, e2) -> VInt (evali e1 env * evali e2 env)
    | Bop(Div, e1, e2) -> VInt (evali e1 env / evali e2 env)
    | Bop(Mod, e1, e2) -> VInt (evali e1 env mod evali e2 env)
    | Bop(And, e1, e2) -> VBool (evalb e1 env && evalb e2 env)
    | Bop(Or, e1, e2) -> VBool (evalb e1 env || evalb e2 env)
    | Bop(Lt, e1, e2) -> VBool (evali e1 env < evali e2 env)
    | Bop(Le, e1, e2) -> VBool (evali e1 env <= evali e2 env)
    | Bop(Eq, e1, e2) -> VBool (evali e1 env == evali e2 env)
    | Bop(Neq, e1, e2) -> VBool (evali e1 env != evali e2 env)
    | If(e0, e1, e2) -> if evalb e0 env then evalv e1 env else evalv e2 env
    | Let(id, e1, e2) -> 
        let ptr = VPtr(new_ptr()) in
        let env2 = Env.add id ptr env in
        let data = VClos(id, e1, env2) in
        let _ = Hashtbl.add mem ptr data in
        evalv e2 env2
    | Var(x) -> 
        (* let _ = Printf.printf "OK" in *)
        let ptr = Env.find x env in
        let VClos(id, e, env) = Hashtbl.find mem ptr in
        (evalv e env)

  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e: expr) (env: value Env.t): int = 
    match eval e env with
    | VInt n -> n
    | _ -> assert false
    
  (* Évaluation d'une expression dont la valeur est supposée booléenne *)
  and evalb (e: expr) (env: value Env.t): bool = 
  match eval e env with
  | VBool b -> b
  | _ -> assert false
  
  (* Évaluation d'une expression dont la valeur est supposée booléenne ou entière *)
  and evalv (e: expr) (env: value Env.t) =
    match eval e env with
    | VBool b -> VBool b
    | VInt n -> VInt n
    | VUnit -> VUnit
    | _ -> assert false

in

  eval p.code Env.empty
