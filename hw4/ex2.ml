type require = id * (cond lis
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list

and gift = int
and id = A | B | C | D | E

let a = Array.make 5 (gift list) in
Array.init a 5 (fun x -> [])
let c = ref 0 in
let length = ref 0 in
let idtonum id = (
  match id with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
) in
let rec shoppingList rlist = 
  length := List.length rlist;
  match rlist with
  | [] -> [(A, Array.get a 0), (B, Array.get a 1), (C, Array.get a 2), (D, Array.get a 3), (E, Array.get a 4)]
  | h::t -> (
    let idnum = idtonum (fst h) in
    let cur = Array.get a idnum in
    match (snd h) with
    | Item glist -> (
      let insert num = (
        if not (List.mem num cur)
        then Array.set a idnum num::cur
      ) in
      List.iter insert glist;
      c := 0;
      length := !length - 1;
      shoppingList t
    )
    | Same subid -> 
        let subidnum = idtonum subid in
        let subarr = Array.get a subidnum in
        let checkgift giftid = (
          let b = List.mem giftid cur in
          (if not b then Array.set a idnum giftid::cur);
          b
        )
        let b = List.for_all checkgift subarr in
        (if b then c := !c + 1 else c := 0);
        if !c = length then shoppingList [] else shoppingList t @ h 

