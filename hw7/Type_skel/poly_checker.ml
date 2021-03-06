(*
 * SNU 4190.310 Programming Languages 2017 Fall
 * Type Checker Skeleton
 *)

open M

let rec etos : M.exp -> string = function
  | M.CONST c -> (
    let ss = "const: (" in
    match c with
    | M.S s -> ss ^ "string: " ^ s ^ ")"
    | M.N n -> ss ^ "num: " ^ string_of_int n ^ ")"
    | M.B b -> ss ^ "bool: " ^ string_of_bool b ^ ")"
  )
  | M.VAR x -> "var: " ^ x
  | M.FN (x, e) -> "fn: (" ^ x ^ ", " ^ etos e ^ ")"
  | M.APP (e1, e2) -> "app(" ^ etos e1 ^ ", " ^ etos e2 ^ ")"
  | M.LET (dec, e2) -> (
    let ss = "let " in
    match dec with
    | M.VAL (x, e1) -> ss ^ x ^ " = " ^ etos e1 ^ " in\n" ^ etos e2
    | M.REC (f, x, e1) -> ss ^ "rec" ^ f ^ " = " ^ etos e1 ^ " in\n" ^ etos e2
  )
  | M.IF (e1, e2, e3) -> "if (" ^ etos e1 ^ ") then " ^ etos e2 ^ " else " ^ etos e3 
  | M.BOP (op, e1, e2) -> "op (" ^ etos e1 ^ ", " ^ etos e2 ^ ")"
  | M.SEQ (e1, e2) -> etos e1 ^ ";\n" ^ etos e2
  | M.PAIR (e1, e2) -> "(" ^ etos e1 ^ ", " ^ etos e2 ^ ")"
  | M.FST e -> "fst (" ^ etos e ^ ")"
  | M.SND e -> "snd (" ^ etos e ^ ")"
  | _ -> "unimplemented"

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TSimple of var
  | TSimple_ of var
  | TPrint
  (* Modify, or add more if needed *)

let rec ttos : typ -> string = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "string"
  | TPair (t1, t2) -> "pair (" ^ ttos t1 ^ ", " ^ ttos t2  ^ ")"
  | TLoc t -> "loc: " ^ ttos t
  | TFun (t1, t2) -> "fun(" ^ ttos t1 ^ ", " ^ ttos t2 ^ ")"
  | TVar v -> "var: " ^ v
  | TSimple v -> "Simple " ^ v
  | TSimple_ v -> "sim_: " ^ v
  | _ -> ""


type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (typ list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  (*"x_" ^*) (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> typ list = fun typ ->
  match typ with
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar _ | TSimple _ | TSimple_ _ -> [typ]
  | _ -> []

let ftv_of_scheme : typ_scheme -> typ list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> typ list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
    | TPrint -> let _ = print_string (x ^ " -> " ^ ttos t ^ " ") in t'
    | TSimple_ x' | TSimple x' -> if (x = x') then t else t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun a -> match a with TVar _ -> TVar (new_var()) | TSimple _ -> TSimple (new_var()) | TSimple_ _ -> TSimple_ (new_var()) | _ -> raise (M.TypeError "gen error!")) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> match alpha with TVar x | TSimple x | TSimple_ x -> (make_subst x beta @@ acc_subst) | _ -> raise (M.TypeError "gen error!!"))
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let rec u : (typ * typ) -> subst = fun (t1, t2) ->
  match (t1, t2) with
  | (TVar a, TVar b) when a = b -> empty_subst (*FIXME*)
  | (TVar a, TVar b) -> if int_of_string a > int_of_string b then make_subst a t2 else make_subst b t1
  | (TVar a, t) | (t, TVar a) when not (List.exists (fun v -> v = TVar a) (ftv_of_typ t)) -> make_subst a t
  | (TSimple_ a, t) | (t, TSimple_ a) -> (
    match t with
    | TInt | TBool | TString | TLoc _ | TSimple _-> make_subst a t
    | TSimple_ b -> if int_of_string a > int_of_string b then make_subst a t else make_subst b (TSimple_ a)
    | _ -> raise (M.TypeError ("simple error"))
  )
  | (TSimple a, t) | (t, TSimple a) -> (
    match t with
    | TInt | TBool | TString -> make_subst a t
    | TSimple b -> if int_of_string a > int_of_string b then make_subst a t else make_subst b (TSimple a)
    | _ -> raise (M.TypeError ("simple error"))
  )
  | (TFun (t1, t2), TFun (t1', t2')) -> 
    let s = u (t1, t1') in
    let s' = u (s t2, s t2') in
    s' @@ s
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst 
  | (TPair (t1, t2), TPair (t1', t2')) -> 
    let s = u (t1, t1') in
    let s' = u (s t2, s t2') in
    s' @@ s
  | (TLoc t1, TLoc t2) -> u (t1, t2)
  | (t1, t2) ->raise (M.TypeError ("unify error: " ^ ttos t1 ^ ", " ^ ttos t2))


let rec expan : M.exp -> bool = function
  | M.CONST _| M.VAR _ | M.FN _ | M.READ -> false
  | M.MALLOC _ | M.APP _ -> true
  | M.BANG e | M.FST e | M.SND e | M.WRITE e -> expan e
  | M.ASSIGN (e1, e2) | M.LET (M.VAL (_, e1), e2) | M.LET (M.REC (_, _, e1), e2) | (M.BOP (_, e1, e2)) | M.PAIR (e1, e2) |  M.SEQ (e1, e2) -> expan e1 || expan e2
  | M.IF (e1, e2, e3) -> expan e1 || expan e2 || expan e3


let rec w : (typ_env * M.exp) -> (subst * typ) = function
  | (_, M.CONST c) -> (
    match c with
    | M.S s -> (empty_subst, TString)
    | M.N n -> (empty_subst, TInt)
    | M.B b -> (empty_subst, TBool)
  )
  | (env, M.VAR x) -> (
    match List.assoc x env with
    | SimpleTyp t -> (empty_subst, t)
    | GenTyp (aphs, t) ->  (
      match subst_scheme empty_subst (GenTyp (aphs, t)) with
      | GenTyp (_, t) -> (empty_subst, t)
      | _ -> raise (M.TypeError "var error")
    )
  )

  | (env, M.FN (x, e)) ->
    let b = SimpleTyp (TVar (new_var())) in(* FIXME *)
    let (s1, t1) = w ((x, b)::env, e) in (
      match subst_scheme s1 b with
      | SimpleTyp t -> (*let _ = print_endline ("FN " ^ ttos t1) in*) (s1, TFun(t, t1))
      | _ -> raise (M.TypeError "new typscheme")
    )
  | (env,M.APP (e1, e2)) -> (
    let (s1, t1) =w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    let b = TVar (new_var()) in
    (*let _ = print_endline ("in M.App s2 t1: " ^ ttos (s2 t1) ^ ", t2: " ^ ttos t2) in*)
    let s3 = u (s2 t1, TFun (t2, b)) in
    (*let _ = (s3 @@ s2 @@ s1) TPrint in
    let _ = print_newline() in*)
    (s3 @@ s2 @@ s1, s3 b)
  )
  | (env, M.LET (dec, e2)) -> (
    match dec with
    | M.VAL (x, e1) ->
      let (s1, t1) = w (env, e1) in
      let s1env = subst_env s1 env in
      if (not (expan(e1))) 
      then
        let (s2, t2) = w ((x, generalize s1env t1)::s1env, e2) in
        (s2 @@ s1, t2)
      else 
        let (s2, t2) = w ((x, SimpleTyp t1)::s1env, e2) in
        (s2 @@ s1, t2)
    | M.REC (f, x, e1) ->
      let b = TVar (new_var()) in
      let (s1', t1') = w ((f, SimpleTyp b)::env, M.FN (x, e1)) in
      (*let _ = print_endline ("in M.REC s1' b: " ^ ttos (s1' b) ^ ", t1': " ^ ttos t1') in*)
      let s2' = u (s1' b, t1') in
      let (s1, t1) = (s2' @@ s1', s2' t1') in
      let s1env = subst_env s1 env in
      if (not (expan (M.FN (x, e1))))
      then
        let (s2, t2) = w ((f, generalize s1env t1)::s1env, e2) in
        (s2 @@ s1, t2)
      else
        let (s2, t2) = w ((f, SimpleTyp t1)::s1env, e2) in
        (s2 @@ s1, t2)
  )
  (*| (env, M.LET (M.REC (f, x, e1), e2)) -> 
    let b = TVar (new_var()) in
    let (s1, t1) = w ((f, SimpleTyp b)::env, M.FN (x, e1)) in
    let s2 = u (s1 b, t1) in
    (s2 @@ s1, s2 t1)
*)
  | (env, M.IF (e1, e2, e3)) -> (
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    let (s3, t3) = w (subst_env s2 (subst_env s1 env), e3) in
    match t1 with
    | TBool -> (s3 @@ s2 @@ s1, s3 t2)
    | TVar x -> (make_subst x TBool @@ s3 @@ s2 @@ s1,s3  t2)
    | _ -> raise (M.TypeError "if error")
  )
  | (env, M.BOP (op, e1, e2)) -> (
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    (*let _ = print_endline ("bop: " ^ ttos (t1) ^ ", " ^ ttos (t2)) in (*FIXME*)*)
    match op with
    | M.ADD | M.SUB -> (
      match (t1, t2) with
      | (TInt, TInt) -> (s2 @@ s1, TInt)
      | (TVar x, TInt) | (TInt, TVar x) -> (make_subst x TInt @@ s2 @@ s1, TInt)
      | (TVar x, TVar x') -> ((make_subst x TInt) @@ (make_subst x' TInt) @@ s2 @@ s1, TInt)
      | _ -> raise (M.TypeError ("operation typerror: " ^ (ttos t1) ^ " " ^ (ttos t2)))
    )
    | M.AND | M.OR -> (
      match (t1, t2) with
      | (TBool, TBool) -> (s2 @@ s1 @@ empty_subst, TBool)
      | (TVar x, TBool) | (TBool, TVar x) -> (make_subst x TBool @@ s2 @@ s1, TBool)
      | (TVar x, TVar x') -> ((make_subst x TBool) @@ (make_subst x' TBool @@ s2 @@ s1), TBool)
      | _ -> raise (M.TypeError ("operation typerror: " ^ (ttos t1) ^ " " ^ (ttos t2)))
    )
    | M.EQ -> (
      match (t1, t2) with
      | (TInt, TInt) | (TBool, TBool) | (TString, TString) | (TLoc _, TLoc _) -> (s2 @@ s1 @@ empty_subst, TBool)
      | (TVar x, TInt) | (TVar x, TBool) | (TVar x, TString) -> (make_subst x t2 @@ s2 @@ s1, TBool)
      | (TInt, TVar x) | (TBool, TVar x) | (TString, TVar x) -> (make_subst x t1 @@ s2 @@ s1, TBool)
      | (TLoc _, TVar x) | (TVar x, TLoc _) -> 
        let t = TVar (new_var()) in (* FIXME*)
        (make_subst x (TLoc t) @@ s2 @@ s1, TBool)
      | (TVar x, TVar x') -> ((make_subst x (TSimple_ (new_var()))) @@ (make_subst x' (TVar x)) @@ s2 @@ s1, TBool)
      | _ -> raise (M.TypeError ("operation typerror: " ^ (ttos t1) ^ " " ^ (ttos t2)))
    )
  )
  | (env, M.READ) -> (empty_subst, TInt)
  | (env, M.WRITE e) -> (
    let (s, t) = w (env, e) in
    match t with
    | TInt | TBool | TString | TSimple _ -> (s, t)
    | TVar x -> (make_subst x (TSimple (new_var())) @@ s, t)
    | TSimple_ x -> (make_subst x (TSimple x) @@ s, t)
    | _ -> raise (M.TypeError ("write error " ^ ttos t))
  )
  | (env, M.MALLOC e) -> 
    let (s, t) = w (env, e) in
    (s, TLoc t)

  | (env, M.ASSIGN (e1, e2)) -> (
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    match (t1, t2) with
    | (TLoc t, t') -> (s2 @@ s1, t)
    | _ -> raise (M.TypeError ("assign error" ^ ttos t1 ^ " " ^ ttos t2))
  )
  | (env, M.BANG e) -> (
    let (s, t) = w (env, e) in
    match t with
    | TLoc t -> (s, t)
    | TVar x -> 
      ((make_subst x (TLoc t)) @@ s, t)
    | _ -> raise (M.TypeError "bang error")
  )
  | (env, M.SEQ (e1, e2)) -> 
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    (s2 @@ s1, t2)

  | (env, M.PAIR (e1, e2)) -> 
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    ((*empty_subst*)s2 @@ s1, TPair (t1, t2))

  | (env, M.FST e) -> (
    let (s, t) = w (env, e) in
    match t with 
    | TPair (t1, t2) -> (empty_subst, t1) (*FIXME*)
    | TVar x -> 
      let t' = TVar (new_var()) in
      (make_subst x (TPair (t', TVar (new_var()))), t')
    | t -> raise (M.TypeError ("fst error: " ^ ttos t))
  )
  | (env, M.SND e) -> (
    let (s, t) = w (env, e) in
    match t with 
    | TPair (t1, t2) -> (empty_subst, t2) (*FIXME*)
    | TVar x -> 
      let t' = TVar (new_var()) in
      (make_subst x (TPair (TVar (new_var()), t')), t')
    | t -> raise (M.TypeError ("snd error: " ^ ttos t))
  )

let rec ttoty : (subst * typ) -> M.typ = function
  | (_, TInt) -> M.TyInt
  | (_, TBool) -> M.TyBool
  | (_, TString) -> M.TyString
  | (s, TPair(t1, t2)) -> M.TyPair (ttoty (s, t1), ttoty (s, t2))
  | (s, TLoc t) -> M.TyLoc (ttoty (s, t))
  | (s, TVar v) -> ttoty (s, s (TVar v))
  | (s, TSimple_ v) -> ttoty (s, s (TSimple_ v))
  | _ -> raise (M.TypeError "unexpected type")



(* TODO : Implement this function *)
let check : M.exp -> M.typ = fun exp -> 
  (*let _ = print_endline (etos exp)in *)
  let (s, t) = (w ([], exp)) in 
(*  let _ = print_endline (ttos t) in*)
  ttoty (s, t)
