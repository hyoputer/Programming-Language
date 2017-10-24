type require = id * (cond list)
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list

and gift = int
and id = A | B | C | D | E

let a = (Array.make 5 [])
let c = ref 0
let idtonum id = (
  match id with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
)
let rec getglist cond = (
  match cond with
  | Items glist -> glist
  | Same id -> Array.get a (idtonum id)
  | Common (a, b) -> (
    let aglist = getglist a in
    let bglist = getglist b in
    List.filter (fun x -> List.mem x bglist) aglist 
  )
  | Except (c, glist) -> (
    let cglist = getglist c in
    List.filter (fun x -> not(List.mem x glist)) cglist
  )
)
let rec shoppingList : require list -> (id * gift list) list  = fun rlist -> (
  let getlength num r = num + List.length (snd r) in
  let length = List.fold_left getlength 0 rlist in
  let next = ref rlist in
  match rlist with
  | [] -> (
    let temp = Array.copy a in
    Array.fill a 0 5 [];
    c := 0;
    let comp a b = a - b in
    [(A, List.sort comp (Array.get temp 0)); (B, List.sort comp (Array.get temp 1)); (C, List.sort comp (Array.get temp 2)); (D, List.sort comp (Array.get temp 3)); (E, List.sort comp (Array.get temp 4))]
  )
  | h::t -> (
    let idnum = idtonum (fst h) in
    next := t;
    let reqiter cond = (
      match cond with
      | Items glist -> (
        let insert num = (
          let cur = Array.get a idnum in
          if not (List.mem num cur)
          then (Array.set a idnum (num::cur)) else ()
          ) in
        List.iter insert glist;
        c := 0;
        )
      | _ -> (
        let glist = getglist cond in
        let checkgift b giftid = (
          let cur = Array.get a idnum in
          let b' = List.mem giftid cur in
          (if not b' then Array.set a idnum (giftid::cur) else ());
          b' && b
          ) in
        let b = List.fold_left checkgift true glist in 
        (if b then c := !c + 1 else c := 0);
        if !c = length then next := [] else (next := !next @ (((fst h), cond::[])::[]))
        )
    ) in
    List.iter reqiter (snd h);
    shoppingList !next
  )
)
(*let _ =
  let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])] in
  assert ((shoppingList []) = emptyL);
  print_endline "0";

  assert ((shoppingList [
    (A, []); (B, []); (C, []); (D, []); (E, []);
    ]) = emptyL);
    print_endline "1";

    assert ((shoppingList [
      (A, [Same B]); (B, [Same C]); (C, [Same D]); (D, [Same E]); (E, [Same A]);
      ]) = emptyL);
      print_endline "2";

      assert ((shoppingList [
        (A, [Items [1;2;3]]); (B, [Items [2;3;4]]);
        (C, [Items [3;4;1]]); (D, [Items [4;1;2]]);
        (E, [Items [1;2;3;1;2;3]]);
        ]) = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]);
        print_endline "3";

        assert ((shoppingList [
          (A, [Items [1;2;3]]);
          (B, [Same A]);
          (C, [Same A; Items[1;2]]);
          (D, [Same A; Items[4]]);
          (E, [Same D]);
          ]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, [1; 2; 3]); (D, [1; 2; 3; 4]); (E, [1; 2; 3; 4])]);
          print_endline "4";

          assert ((shoppingList [
            (A, [Common (Items [1;2;3], Items [2;1;3])]);
            (B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]);
            (C, [Common (Items [1;2;3], Items [4;5;6])]);
            (D, [Common (Items [3;2;1], Items [1])]);
            (E, [Common (Items [1;2;3], Items [])]);
            ]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]);
            print_endline "5";

            assert ((shoppingList [
              (B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]);
              (E, [Common (Items [], Items [])]);
              (D, [Common (Items [1], Items [1])]);
              ]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]);
              print_endline "6";

              assert ((shoppingList [
                (A, [Except (Items [3;2;1], [3;2;1])]);
                (B, [Except (Items [2;1;3], [])]);
                (C, [Except (Items [2;1;3], [1;2;3;4;5;6])]);
                (D, [Except (Items [], [2;1;3])]);
                (E, [Except (Items [], [])]);
                ]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, []); (E, [])]);
                print_endline "7";

                assert ((shoppingList [
                  (A, [Common (Common (Same B, Same C), Common (Same D, Same E))]);
                  (B, [Common (Same C, Common (Same D, Except (Same E, [5])))]);
                  (C, [Same D; Items[7;8]]);
                  (D, [Except (Same E, [1;2;3])]);
                  (E, [Items [1;2;3;4;5]]);
                  ]) = [(A, [4]); (B, [4]); (C, [4; 5; 7; 8]); (D, [4; 5]); (E, [1; 2; 3; 4; 5])]);
                  print_endline "8";

                  assert ((shoppingList [
                    (A, [Same B; Same C]);
                    (B, [Except (Same C, [1;2;3]); Same D]);
                    (C, [Items [1;2;3]; Items [3;4;5]; Common (Same A, Items [6;7])]);
                    (D, [Same E]);
                    (E, [Same D; Items[6;8]]);
                    ]) = [(A, [1; 2; 3; 4; 5; 6; 8]); (B, [4; 5; 6; 8]); (C, [1; 2; 3; 4; 5; 6]); (D, [6; 8]); (E, [6; 8])]);
                    print_endline "9";

                    assert ((shoppingList [
                      (A, [Common (Same B, Common (Except (Items [1;2;3;4;5], [1;3;5]), Same C)); Items [2;4;8]]);
                      (B, [Except (Except (Except (Same A, [1]),[1;2]),[3]); Items [3;6;9]]);
                      (C, [Same A; Same B; Same D; Same E]);
                      (D, [Items [10]; Common (Same A, Same D); Items [5]]);
                      (E, [Common (Same C, Common (Same A, Common (Same D, Same B)))])
                      ]) = [(A, [2; 4; 8]); (B, [3; 4; 6; 8; 9]); (C, [2; 3; 4; 5; 6; 8; 9; 10]); (D, [5; 10]); (E, [])]);
                      print_endline "10";

                      assert ((shoppingList [
                        (A, [Items [1;2;3;1;2;3]]);
                        (D, [Items [5;5;5;5;5]]);
                        (A, [Same D]);
                        (E, [Except (Items [1;2;3;1;2;3], [1;2;3])]);
                        (A, [Items [1;2;3;4]]);
                        ]) = [(A, [1; 2; 3; 4; 5]); (B, []); (C, []); (D, [5]); (E, [])]);

                        print_endline "pass all tests";*)
