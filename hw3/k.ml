(*
 * SNU 4190.310 Programming Languages 2017 Fall
 *  K- Interpreter Skeleton Code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM n -> (Num n, mem)
    | UNIT -> (Unit, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | VAR x ->
      let l = lookup_env_loc env x in
      let v = Mem.load mem l in
      (v, mem)
    | RECORD rlist -> (
      match rlist with
      | [] -> (Unit, mem)
      | _ -> (
        let recf (re, mem) (id, exp) = (
          let (v, mem') = eval mem env exp in
          let (l, mem'') = Mem.alloc mem' in
          (Record (fun x -> if x = id then l else (value_record re) x), Mem.store mem'' l v)
        ) in
        List.fold_left recf (Record (fun x -> raise (Error "Unbound")), mem) rlist
      )
    )
    | ADD (e1, e2) ->
      let (v1, mem')  = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 + value_int v2), mem'')
    | SUB (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 - value_int v2), mem'')
    | MUL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 * value_int v2), mem'')
    | DIV (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 / value_int v2), mem'')
    | EQUAL (e1, e2) -> (
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      match (v1, v2) with
      | (Num a, Num b) -> (Bool (a = b), mem'')
      | (Bool a, Bool b) -> (Bool (a = b), mem'')
      | (Unit, Unit) -> (Bool true, mem'')
      | (_, _) -> (Bool false, mem'')
    )
    | LESS (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem env e2 in
      (Bool (value_int v1 < value_int v2), mem'')
    | NOT x ->
      let (b, mem') = eval mem env x in
      (Bool (not (value_bool b)), mem')
    | ASSIGNF (e1, x, e2) ->
      let (r, mem') = eval mem env e1 in
      let (v, mem'') = eval mem' env e2 in
      (v, Mem.store mem'' ((value_record r) x) v)
    | FIELD (e, x) ->
      let (r, mem') = eval mem env e in
      (Mem.load mem' ((value_record r) x), mem')
    | SEQ (e1, e2) ->
      let (_, mem') = eval mem env e1 in
      let (v, mem'') = eval mem' env e2 in
      (v, mem'')
    | IF (c, e1, e2) -> ( 
      let (b, mem') = eval mem env c in
      match (value_bool b) with
      | true -> eval mem' env e1 
      | false -> eval mem' env e2
    )
    | WHILE (e1, e2) -> (
      let (b, mem') = eval mem env e1 in
      match (value_bool b) with
      | false -> (Unit, mem')
      | true -> (
        let (v1, mem1) = eval mem' env e2 in
        eval mem1 env (WHILE (e1, e2))
      )
    )
    | LETF (x, vlist, e1, e2) ->
      eval mem (Env.bind env x (Proc (vlist, e1, env))) e2
    | CALLV (f, elist) -> (
      let exf (vlist, mem) e = (
        let tuple = eval mem env e in
        ((fst tuple)::vlist, snd tuple)
      ) in
      let (vlist, mem') = List.fold_left exf ([], mem) elist in
      let (idlist, ex, env') = lookup_env_proc env f in
      let idviter (env, mem) id v = (
        let (l, mem') = Mem.alloc mem in
        (Env.bind env id (Addr l), Mem.store mem' l v)
      ) in
      try let (env'', mem'') = List.fold_left2 idviter (env', mem') idlist (List.rev vlist) in
      eval mem'' env'' (LETF (f, idlist, ex, ex))
      with Invalid_argument  _-> raise (Error "InvalidArg")
    )
    | CALLR (f, ylist) -> (
      let (xlist, ex, env') = lookup_env_proc env f in
      let changenv env' x y = Env.bind env' x (Env.lookup env y) in
      try let env'' = List.fold_left2 changenv env' xlist ylist in
      eval mem env'' (LETF (f, xlist, ex, ex))
      with Invalid_argument _ -> raise (Error "InvalidArg")
    )
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
