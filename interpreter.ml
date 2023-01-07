(* Interprète Mini-ML *)

open Mml

(* Environnement : associe les pointeurs aux noms des variables *)
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

  (* Initialisation de la mémoire globale qui stocke les heap_value *)
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
    | Int n  ->  VInt n
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
      let env_extended = Env.add id ptr env in
      let data = VClos(id, e1, env) in
      let _ = Hashtbl.add mem ptr data in
      evalv e2 env_extended
    | Var(x) -> 
      let ptr = Env.find x env in
      let VClos(_, e, env) = Hashtbl.find mem ptr in
      evalv e env
    | Seq(e1, e2) -> (evalv e1 env); (evalv e2 env)
    | Fun(x, _, e) -> 
      let ptr = VPtr(new_ptr()) in
      let env_extended = Env.add x ptr env in
      let data = VClos(x, e, env_extended) in 
      let _ = Hashtbl.add mem ptr data in
      ptr
    | App(f, e) -> 
      (* On récupère le pointeur de la fonction f *)
      let ptrf = evalv f env in

      (* On récupère la cloture fonctionnelle de f *)
      let VClos(funx, expfun, envfun) = Hashtbl.find mem ptrf in

      (* 
        On étend l'environnement où le paramètre x de f est associée à la valeur de e.
        x -> eval(e)
      *)
      let ptrx = VPtr(new_ptr()) in
      let envx = Env.add funx ptrx envfun in
      let datax = VClos(funx, e, envfun) in
      let _ = Hashtbl.add mem ptrx datax in

      (* On évalue l'expression de f dans l'environnement de x *)
      evalv expfun envx      
    | Fix(f, _, e) ->
      let ptr = VPtr(new_ptr()) in
      let envf = Env.add f ptr env in  
      let dataf = VClos(f, e, envf) in
      let _ = Hashtbl.add mem ptr dataf in

      (* On met à jour l'environnement de la cloture fonctionnelle en y ajoutant une association f -> ptr *)
      let VClos(_, _, env) = dataf in
      let new_env = Env.add f ptr env in
      let new_dataf = VClos(f, e, new_env) in
      let _ = Hashtbl.replace mem ptr new_dataf in

      ptr

    (*| GetF(e, x) -> 
      let eval_e = eval e env (* e reconnu comme Var(x) : renvoie pointeur adresse *)
      in
      let l = Hashtbl.find mem eval_e (* heap_value *) 
      in 
      match l with
      |VStrct(hash) -> Hashtbl.find hash x
      |_ -> assert false 


      | SetF(e1, x, e2) -> let eval_e1 = eval e1 env and eval_e2 = evalv e2 env 
      in
      let l = Hashtbl.find mem eval_e1 
      in 
      match l with
      |VStrct(hash) -> Hashtbl.replace hash x eval_e2
      |_ -> assert false 


    *)
    | Strct(s) ->  
      let rec new_hash_strct s hash = match s with 
        | h::t -> Hashtbl.add hash (fst h) (snd h); new_hash_strct t hash
        | [] -> ()
      in
      let ptr = VPtr(new_ptr()) and hash = Hashtbl.create 2 and st = evals s env in
      new_hash_strct st hash;
      let data = VStrct(hash) in
      let _ = Hashtbl.add mem ptr data in
      ptr

  (*| Enum(c) -> c*)


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

  (* Évaluation d'une expression dont la valeur est supposée de type value *)
  and evalv (e: expr) (env: value Env.t) =
    match eval e env with
    | VBool b -> VBool b
    | VInt n -> VInt n
    | VUnit -> VUnit
    | VPtr p -> VPtr p
    | _ -> assert false

  and evals s (env: value Env.t) =
    let l = [] in 
    match s with 
    |h::t -> (fst h,evalv (snd h) env)::evals t env
    |[] -> l  
  in

  eval p.code Env.empty
