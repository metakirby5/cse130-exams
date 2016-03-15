(* 1a *)
let rec insert l i =
  match l with
  | h :: t ->
      if i <= h
      then i :: h :: t
      else h :: insert t i
  | [] -> [i];;

assert (insert [] 10 = [10]);;
assert (insert [1;2;3;4] 3 = [1;2;3;3;4]);;
assert (insert [10;15;20;30] 40 = [10;15;20;30;40]);;
assert (insert [10;15;20;30] 5 = [5;10;15;20;30]);;

(* 1b *)
let insertion_sort =
  List.fold_left insert [];;

assert (insertion_sort [5;2;3;1;5;3;4] = [1;2;3;3;4;5;5]);;

(* 2 *)
type expr =
  | Var of string
  | Const of int
  | Plus of expr * expr;;

let rec simpl e =
  match e with
  (* case for plus: *)
  | Plus (e1, e2) -> (
    let e1' = simpl e1 in
    let e2' = simpl e2 in
    match (e1', e2') with
    | (Const c1, Const c2) -> Const (c1 + c2)
    | _ -> Plus(e1', e2')
  )
  (* all other cases: *)
  | _ -> e;;

assert (simpl (Plus (Const 4, Const 7)) = Const 11);;
assert (simpl (Plus (Plus (Const 20, Const 10), Var "a")) =
  Plus (Const 30, Var "a"));;
assert (simpl (Plus (Plus (Const 4, Const 10), Const 7)) =
  Const 21);;
assert (simpl (Plus (Const 4, Var "a")) =
  Plus (Const 4, Var "a"));;
assert (simpl (Plus (Var "a", Var "b")) = 
  Plus (Var "a", Var "b"));;
assert (simpl (Plus (Plus (Var "a", Const 10), Const 7)) =
  simpl (Plus (Plus (Var "a", Const 10), Const 7)));;
