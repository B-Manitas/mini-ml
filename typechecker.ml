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

    and check_strct s type_s tenv = (* s = la liste de 'string*expression'*)
    match (s, type_s) with
    |h1::t1, h2::t2 -> check_error (snd h1) (snd(fst(triple_access h2))) tenv; check_strct t1 t2 tenv;
    |[], []-> ()
    |[], h2::t2 -> error "not same struct"
    |h1::t1, [] -> error "not same struct"
    |_, h::t -> error "not a struct" (* pour le cas où on accede à s.x le champ x d'une struct s *)

and check_enum c prog_enumtypes = 
    let rec search_for_c_in c l =  (* c un "sous_type" enumere, l la liste des types enumeres avec leurs "sous_types associés" *)
    match l with
    |h::t -> if c = h  then true else search_for_c_in c t 
    |[] -> false
in
    match prog_enumtypes with (* snd h la liste des sous_types enumerés, fst h le type enum auquel appartient c *)
    |h::t -> if (search_for_c_in c (snd h)) then fst h else check_enum c t
    |[] -> error "c n'est un sous_types enumeres" 

and triple_access t = (* tranforme le triple en pair pour avoir acces plus facilement *)
  match t with
    |(a, b, c) -> ((a, b), c) 

and find_type_strct1 s prog_types = (* à partir de s = la liste de 'string*expression'*)
  let rec f_aux l1 l2 = 
    match (l1, l2) with
    |h1::t1, h2::t2 -> if fst h1 = fst(fst(triple_access h2)) (*&& type_expr (snd h1) tenv =  (triple_access h2 2)*) then f_aux t1 t2 else false (* verifie si on a meme champ et meme type de champ *)
    |[], [] -> true
    |[], h2::t2 -> false
    |h1::t1, [] -> false
  in  
  match prog_types with (*  list de pair : string  *  list de string typ bool       *)
    |h::t -> if f_aux s (snd h) then h else find_type_strct1 s t (*snd h = list string typ bool *)
    |[] -> error " "

and find_type_strct2 v prog_types = (*  *)
    let type_v = match v with
    |TStrct(s) -> s
    |_ -> error " not the type expected "
  in
    match prog_types with 
    |h::t -> if (fst h = type_v) then h else find_type_strct2 v t 
    |[] -> error " le type s n'est pas dans les types du programme "
  
and find_type_var x strct =
    match strct with
    |h::t -> if x = fst(fst(triple_access h)) then snd(fst(triple_access h)) else find_type_var x t 
    |[] -> error " la variable x n'est pas une clé de la structure "

and champ_acces s x = (* accede au champ x de la structure de type s : list string expr *)
    match s with 
    |h::t -> if (fst h = x) then snd h else champ_acces t x
    |[] -> error "x n'est pas un champ de str "

and verif_champ_mutable type_e2 x strct =
    match strct with
    |h::t -> 
    if x = fst(fst(triple_access h)) then 
      if snd(triple_access h) then 
        if type_e2 != snd(fst(triple_access h)) then 
          error "e2 pas le meme type que le champ x" 
        else () 
      else error "pas mutable"  
    else verif_champ_mutable type_e2 x t 
    |[] -> error "x n'est pas un champ de strct "
    
(*(* on veut separer les definitions des structures des definitions des types enumeres*)     
and separate l type_strct type_enum = (*l = prog.types : liste de pair (a, b) avec a:ident b:soit list de string ou soit list de string*typ*bool*) 
 

let identify p = (*p est une pair string * soit list de string ou soit list de string*typ*bool *)
 let verif l = match l with 
 |(a, b, c) -> "struct"
 |_ -> "enum"
 in
 match p with  
 |(a, b) -> match b with |h::t -> verif h (* on regarde le 1er element de la liste seulement (suffisant) *)
in


match l with 
(* pour chaque pair on le stocke dans soit type_strct ou dans type_enum suivant son type *)
      |h::t-> if (identify h = "enum") then separate t type_strct h::type_enum
                                       else separate t ::type_strct type_enum
      |[] -> (type_strct, type_enum)*)



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
    | Fun(id, tid, e) -> let te = type_expr e (SymTbl.add id tid tenv) in TFun(tid, te)
    | App(f, e) -> check_fun f e tenv
    | Fix(f, t, e) -> let tenv = SymTbl.add f t tenv in type_expr e tenv

    (* e.x impossible, ici on manipule des listes, pas de structure proprement dite *)
  
    (* |GetF(e,x) ->  (* probleme : à partir de e, trouver l'expression de son champ x *)
      let type_e = 
        match e with 
        |Var(v) ->  (find_type_strct2 (type_expr e tenv) prog.types, v.x) 
        |_ -> error "not the type expected" 
    in check_error (snd type_e) (find_type_var x (snd (fst type_e))); type_expr (snd type_e) tenv changer snd type_e pour qu'il renvoie l'expression du champ x *)

    (*|SetF(e1,x,e2) -> let type_e1 = (find_type_strct2 e1 x prog.types tenv) and type_e2 = type_expr e2 tenv in verif_champ_mutable type_e2 x (snd(fst(type_e1))); TUnit*)
    
    |Strct(s) -> let type_s = (find_type_strct1 s prog.types) in check_strct s (snd type_s) tenv; TStrct(fst type_s)
    
    
    (*|Strct(s) -> let strct_types = fst(separate prog.types [] []) and
    type_s = (find_type_strct1 s strct_types) in check_strct s (snd type_s) tenv; TStrct(fst type_s)
    |Enum(c) -> let enum_types = snd(separate prog.types [] []) TEnum(check_enum c enum_types)*) (* verifie si c et un sous-type enumere et renvoie le type enumere auquel il appartient *)

  in

  type_expr prog.code SymTbl.empty
