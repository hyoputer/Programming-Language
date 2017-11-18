(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE] @ trans (K.VAR x)
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.IF (c, e1, e2) -> trans c @ [Sm5.JTR (trans e1, trans e2)] 
    | K.WHILE (e1, e2) ->
      let l = trans e1 in
      let l' = trans e2 in
      let rec fncheck fn l = if List.mem (Sm5.PUSH (Sm5.Id fn)) l then fncheck (fn ^ "w") l else fn in
      let fn = fncheck (fncheck "w" l) l' in
      let vn = fncheck (fncheck "v" l) l' in
      trans (K.LETF (fn, vn, K.IF ((K.VAR vn), K.SEQ (e2, K.CALLV(fn, e1)), K.UNIT), K.CALLV(fn, e1)))
    | K.FOR (x, e1, e2, e3) -> 
      let l = trans e2 in
      let l' = trans e3 in
      let rec vncheck vn l = if List.mem (Sm5.PUSH (Sm5.Id vn)) l then vncheck (vn ^ "v") l else vn in
      let vn = vncheck (vncheck "v" l) l' in
      let vn' = vncheck (vn ^ "v") l' in
      let i = vncheck (vn' ^ "v") l' in
      trans (K.LETV (vn, e1, (K.LETV (vn', e2, K.LETV (i, K.NUM 0, K.IF (K.LESS (K.VAR vn', K.VAR vn), K.UNIT, 
      K.SEQ (
        K.SEQ (
          K.SEQ (
            K.ASSIGN (x, K.VAR vn), 
            K.WHILE (
              K.NOT (
                K.LESS (
                  K.SUB (K.VAR vn', K.NUM 1), K.VAR x
                )
              ), 
              K.SEQ (
                e3, K.ASSIGN (
                  x, K.SEQ (
                    K.ASSIGN (
                      i, K.ADD (K.VAR i, K.NUM 1)
                    ), 
                    K.ADD (K.VAR vn, K.VAR i)
                  )
                )
              )
            )
          ),
          e3
        ), 
        K.UNIT
      )))))))
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) -> 
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans e1)); Sm5.BIND f] @ 
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.CALLV (f, e) -> [Sm5.PUSH(Sm5.Id f); Sm5.PUSH(Sm5.Id f)] @ trans e @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, x) -> [Sm5.PUSH(Sm5.Id f); Sm5.PUSH(Sm5.Id f); Sm5.PUSH (Sm5.Id x); Sm5.LOAD; Sm5.PUSH (Sm5.Id x); Sm5.CALL]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> 
      trans e @ [Sm5.MALLOC; Sm5.BIND "a"; Sm5.PUSH (Sm5.Id "a"); Sm5.STORE; Sm5.PUSH (Sm5.Id "a"); Sm5.LOAD; Sm5.PUT] @
      trans (K.VAR "a") @ [Sm5.UNBIND; Sm5.POP]
    | _ -> failwith "Unimplemented"

end
