(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp
let rec string_of_xexp : xexp -> string = function
  | Num i -> "Num " ^ (string_of_int i)
  | Var x -> "Var " ^ x
  | Fn (x, e) -> "Fn (" ^ x ^ ", " ^ string_of_xexp e ^ ")"
  | App (e1, e2) -> "(" ^ string_of_xexp e1 ^ ", " ^ string_of_xexp e2 ^ ")"
  | If (e1, e2, e3) -> "If " ^ string_of_xexp e1 ^ "then " ^ string_of_xexp e2 ^ "else " ^ string_of_xexp e3
  | Equal (e1, e2) -> string_of_xexp e1 ^ " = " ^ string_of_xexp e2
  | _ -> ""

let count = ref 0
let new_name () = 
  let _ = count := !count + 1 in
  "#" ^ (string_of_int !count)

let rec hl : xexp -> xexp = fun e ->
  let k = new_name () in
  match e with
  | Num n -> Fn ("#exn", Fn (k, App (Var k, Num n)))
  | Var x -> Fn ("#exn", Fn (k, App (Var k, Var x)))
  | Fn (x, e) -> Fn ("#exn", Fn (k, App(Var k, Fn (x, hl e))))
  | App (e1, e2) -> 
    let f = new_name () in
    let v = new_name () in
    Fn ("#exn", 
      Fn (k, 
        App (
          App (hl e1, Var "#exn"),
            Fn (f, 
              App (
                App (hl e2, Var "#exn"),
                  Fn (v, 
                    App (
                        App (
                          App (Var f, Var v), Var "#exn"
                        ), Var k
                    )
                  )
                )            
            )
        )
      )
    )
  | If (e1, e2, e3) -> 
    let v1 = new_name () in
    Fn ("#exn",
      Fn (k, 
        App (
          App (hl e1, Var "#exn"), 
            Fn (v1, 
              If (Var v1, App (App (hl e2, Var "#exn"), Var k), App (App (hl e3, Var "#exn"), Var k))
            )
        )
      )
    )
  | Equal (e1, e2) -> 
    let v1 = new_name () in
    let v2 = new_name () in
    Fn ("#exn",
      Fn (k,
        App (
          App (hl e1, Var "#exn"),
            Fn (v1,
              App (
                App (hl e2, Var "#exn"),
                  Fn (v2, 
                    App (Var k, Equal (Var v1, Var v2))
                  )
              )
            )
        )
      )
    )
  | Raise e -> 
    let v = new_name () in
    Fn ("#exn", Fn (k, App (App (hl e, Var "#exn"), Fn (v, App (Var "#exn", Var v)))))
  | Handle (e1, n, e2) -> 
    let v = new_name () in
    Fn ("#exn", 
      Fn (k, 
        App (
          App (hl e1, 
            Fn ("#", If (Equal (Var "#", Num n), App (App ((hl e2), Var "#exn"), Fn ("#", Var "#")), App ((Var "#exn"), Var "#")))
          ), Fn (v, App (Var k, Var v))
        )
      )
    )

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e -> App (App (hl e, Fn ("#e", Num 201712)), Fn ("#", Var "#"))
