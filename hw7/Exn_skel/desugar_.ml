(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp
let rec hl : (xexp * xexp) -> (xexp * xexp) = fun (hd, e) ->
  match e with
  | Num _
  | Var _ -> (hd, e)
  | Fn (x, e) -> let (hd', e') = hl (hd, e) in (hd', Fn (x, e'))
  | App (e1, e2) -> let (hd', e1') = hl (hd, e1) in let (hd'', e2') = hl(hd', e2) in (hd'', App (e1', e2'))
  | If (e1, e2, e3) -> (hd, If (snd (hl (hd, e1)), snd (hl (hd, e2)), (snd (hl (hd, e3)))))
  | Equal (e1, e2) -> (hd, Equal (snd (hl (hd, e1)), snd (hl (hd, e2))))
  | Raise e -> (hd, App (hd, App (Fn ("#exns", App(hd, snd (hl (hd, e)))), Fn("##", Var "##"))))
  | Handle (e1, n, e2) -> let hd' = Fn ("##", If (Equal (Var "##", Num n), e2, App (hd, Var "##"))) in (hd', App(Fn ("#exns", snd (hl (hd, e1))), Fn("##", If (Equal (Var "##", Num n), snd (hl (hd', e2)), App (Var "#exns", Var "##")))))

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e -> (App (Fn ("#exns", snd (hl (Fn("##", App(Var "#exns", Var "##")), e))), Fn ("##", Num 201712)))
