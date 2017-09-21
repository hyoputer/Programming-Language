module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr =
    | NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

  type environment
  type value

  val emptyEnv : environment
  val eval : environment * expr -> value

  val print_value : value -> unit
end 

module Zexpr : ZEXPR =
struct
  exception Error of string
  type id = string
  type expr =
    | NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

  type environment = (id * expr) list
  type value = int

  let emptyEnv = []
  let rec eval (env, exp) : value = 
    match exp with
    | NUM i -> i
    | PLUS (a, b) -> eval(env, a) + eval(env, b)
    | MINUS (a, b) -> eval(env, a) - eval(env, b)
    | MULT (a, b) -> eval(env, a) * eval(env, b)
    | DIVIDE (a, b) when eval(env, b) = 0 -> raise (Error "divide by zero")
    | DIVIDE (a, b) -> eval(env, a) / eval(env, b)
    | MAX l -> ( 
        let comp a b: int = b - a in
        let ev exp : value = eval (env, exp) in
        match l with
        | [] -> 0
        | _ -> List.hd(List.sort comp (List.map ev l))
    )
    | VAR id -> (
        let f v : bool = (id = (fst v)) in
        eval (env, snd (List.find f env))
    )
    | LET(id, ex1, ex2) 
        when (let f v : bool = (id = fst v) in
        List.exists f env) -> (let f v : bool = (id <> fst v) in
            eval((id, NUM (eval(env, ex1)))::(List.filter f env), ex2)
        )
    | LET(id, ex1, ex2) -> eval ((id, NUM (eval(env,ex1)))::env, ex2)

  let print_value v : unit = print_int v
end 
