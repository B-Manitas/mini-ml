open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))

let type_warning ty_actual ty_expected =
(Printf.printf "Warning: expected %s but got %s.\n" 
          (typ_to_string ty_expected) (typ_to_string ty_actual))

let type_fun ty_actual =
  error (Printf.sprintf "expected function but got %s" 
           (typ_to_string ty_actual))


(* Vérification des types d'un programme *)
let type_prog prog =

  (* Vérifie que l'expression [e] a le type [type] *)
  let rec check_warning e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_warning typ_e typ
    

  and check_error e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
    
  and check_fun f x tenv = 
    let tf = type_expr f tenv in
    let tx = type_expr x tenv in
    begin match tf with 
    | TFun(ta, te) -> check_error x ta tenv; te
    | _ -> type_fun tf
    end

  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Uop(Neg, e) -> 
      check_error e TInt tenv; TInt
    | Uop(Not, e) -> 
      check_error e TBool tenv; TBool
    | Bop((Add | Mul | Sub | Div | Mod), e1, e2) -> 
      check_error e1 TInt tenv; check_error e2 TInt tenv; TInt      
    | Bop((And | Or), e1, e2) -> 
      check_error e1 TBool tenv; check_error e2 TBool tenv; TBool
    | Bop((Lt | Le | Eq | Neq), e1, e2) -> 
      check_error e1 TInt tenv; check_error e2 TInt tenv; TBool
    | Bop((Eq | Neq), e1, e2) -> 
      check_error e2 (type_expr e1 tenv) tenv; TBool
    | If(e0, e1, e2) -> 
      check_error e0 TBool tenv; check_error e1 (type_expr e2 tenv) tenv; (type_expr e2 tenv) 
    | Let(id, e0, e1) -> 
      let type_e1 = type_expr e0 tenv in
      let tenv = SymTbl.add id type_e1 tenv in
      type_expr e1 tenv
    | Var(x) -> SymTbl.find x tenv
    | Seq(e1, e2) -> check_warning e1 TUnit tenv; type_expr e1 tenv
    | Fun(id, tid, e) -> 
      let te = type_expr e (SymTbl.add id tid tenv) in
      TFun(tid, te)
    | App(f, e) -> check_fun f e tenv

  in

  type_expr prog.code SymTbl.empty
