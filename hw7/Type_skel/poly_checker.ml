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
  | TPrint -> ""

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]
  | TPrint -> []

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
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
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let rec u : (typ * typ) -> subst = fun (t1, t2) ->
  match (t1, t2) with
  | (TVar a, t) | (t, TVar a) when not (List.exists (fun v -> v = a) (ftv_of_typ t)) -> make_subst a t 
  | (TFun (t1, t2), TFun (t1', t2')) -> 
    let s = u (t1, t1') in
    let s' = u (s t2, s t2') in
    s' @@ s
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TPair (t1, t2), TPair (t1', t2')) -> (u (t1, t1')) @@ (u (t2, t2'))
  | (TLoc t1, TLoc t2) -> u (t1, t2)
  | (t1, t2) ->raise (M.TypeError ("unify error: " ^ ttos t1 ^ ", " ^ ttos t2))


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
      | SimpleTyp t -> (s1, TFun(t, t1))
      | _ -> raise (M.TypeError "new typscheme")
    )
  | (env,M.APP (e1, e2)) -> (
    let (s1, t1) = w (env, e1) in
    let (s2, t2) = w (subst_env s1 env, e2) in
    let b = TVar (new_var()) in
    let _ = print_endline ("in M.App t1: " ^ ttos t1 ^ " s2 t1: " ^ ttos (s2 t1) ^ ", t2: " ^ ttos t2) in
    let s3 = u (s2 t1, TFun (t2, b)) in
    let _ = s3 TPrint in
    let _ = print_newline() in
    let _ = print_endline (ttos (s3 b)) in
    (s3 @@ s2 @@ s1, s3 b)
  )
  | (env, M.LET (M.VAL (x, e1), e2)) -> (
    let (s1, t1) = w (env, e1) in
    let s1env = subst_env s1 env in
    let (s2, t2) = w ((x, generalize s1env t1)::s1env, e2) in
    (s2 @@ s1, t2)
  )
  | (env, M.IF (e1, e2, e3)) -> (empty_subst, TBool)
  | (env, M.BOP (op, e1, e2)) -> (
    match (op, w (env, e1), w (env, e2)) with
    | (M.ADD, (_, TInt), (_, TInt)) | (M.SUB, (_, TInt), (_, TInt)) -> (empty_subst, TInt)
    | (M.AND, (_, TBool), (_, TBool)) | (M.OR, (_, TBool), (_, TBool))
    | (M.EQ, (_, TInt), (_, TInt)) 
    | (M.EQ, (_, TBool), (_, TBool)) 
    | (M.EQ, (_, TString), (_, TString)) 
    | (M.EQ, (_, TLoc _), (_, TLoc _)) 
      ->(empty_subst, TBool)
    | (M.ADD, (_, TVar x), (_, TInt)) ->
      (make_subst x TInt, TInt)
    | (_, (_, t1), (_, t2)) -> raise (M.TypeError ("operation typerror: " ^ (ttos t1) ^ " " ^ (ttos t2)))
  )
  | (env, M.SEQ (e1, e2)) -> 
    let (_, t) = w (env, e2) in
    (empty_subst, t)

  | (env, M.PAIR (e1, e2)) -> 
    let (_, t1) = w (env, e1) in
    let (_, t2) = w (env, e2) in
    (empty_subst, TPair (t1, t2))

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
    | TPair (t1, t2) -> (empty_subst, t1) (*FIXME*)
    | TVar x -> 
      let t' = TVar (new_var()) in
      (make_subst x (TPair (TVar (new_var()), t')), t')
    | t -> raise (M.TypeError ("snd error: " ^ ttos t))
  )
  | _ -> raise (M.TypeError "Type Checker Unimplemented")

let rec ttoty : (subst * typ) -> M.typ = function
  | (_, TInt) -> M.TyInt
  | (_, TBool) -> M.TyBool
  | (_, TString) -> M.TyString
  | (s, TPair(t1, t2)) -> M.TyPair (ttoty (s, t1), ttoty (s, t2))
  | (s, TLoc t) -> M.TyLoc (ttoty (s, t))
  | (s, TVar v) -> ttoty (s, s (TVar v))
  | _ -> raise (M.TypeError "unexpected type")



(* TODO : Implement this function *)
let check : M.exp -> M.typ = fun exp -> 
  let _ = print_endline (etos exp)in 
  let (s, t) = (w ([], exp)) in 
  (*let _ = print_endline (ttos t) in*)
  ttoty (s, t)
