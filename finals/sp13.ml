(* 1a *)
let count f l =
  let incr a n = if f n then a + 1 else a in
  List.fold_left incr 0 l;;

(* 1b *)
assert (count ((=) 7)                 [1;2;3;4;5;6;7;8;9] = 1);;
assert (count ((!=) 8)                [1;2;3;4;5;6;7;8;9] = 8);;
assert (count ((<) 3)                 [1;2;3;4;5;6;7;8;9] = 6);;
assert (count ((>=) 2)                [1;2;3;4;5;6;7;8;9] = 2);;
assert (count (fun x -> x mod 2 = 0)  [1;2;3;4;5;6;7;8;9] = 4);;

(* 1c *)
let stretch l =
  let dup a n = a @ [n; n] in
  List.fold_left dup [] l;;

assert (stretch [1;2;3;4] = [1;1;2;2;3;3;4;4]);;
assert (stretch ["a";"b";"c"] = ["a";"a";"b";"b";"c";"c"]);;
assert (stretch [0;0;1] = [0;0;0;0;1;1]);;

(* 2 *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree list;;

exception Mismatch;;

let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (h1 :: t1, h2 :: t2) -> (h1, h2) :: (zip t1 t2)
  | _ -> raise Mismatch;;

let rec tree_zip t1 t2 =
  match (t1, t2) with
  | (Node (x1, l1), Node (x2, l2)) ->
      Node ((x1, x2), List.map (fun (c1, c2) -> tree_zip c1 c2) (zip l1 l2))
  | (Empty, Empty) -> Empty
  | _ -> raise Mismatch;;

let t1 = Node (1, [Node (2, []); Node (3, [])]);;
let t2 = Node (4, [Node (5, []); Node (6, [])]);;
tree_zip t1 t2;;
