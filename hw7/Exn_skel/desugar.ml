(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp
let rec hl : (xexp * xexp) -> (xexp * xexp) = fun (hd, e) ->
  match e with
  | Num n -> (hd, Num n)
  | Var x -> (hd, Var x)
  | Fn (x, e) -> let (hd', e') = hl (hd, e) in (hd', Fn (x, e'))
  | App (e1, e2) -> let (hd', e1') = hl (hd, e1) in let (hd'', e2') = hl(hd', e2) in (hd'', App (e1', e2'))
  | If (e1, e2, e3) -> (hd, If (snd (hl (hd, e1)), snd (hl (hd, e2)), (snd (hl (hd, e3)))))
  | Equal (e1, e2) -> (hd, Equal (snd (hl (hd, e1)), snd (hl (hd, e2))))
  | Raise e -> (hd, App (hd, snd (hl (hd, e))))
  | Handle (e1, n, e2) -> let hd' = Fn ("#exn", If (Equal (Var "#exn", Num n), e2, App (hd, Var "#exn"))) in (hd', snd (hl (hd', e1)))

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e -> 
  snd (hl (
    Fn("#exn", Num 201712),
    e
  ))
