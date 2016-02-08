(* 1a *)
let count l x =
  let f acc nex =
    if x = nex then acc + 1 else acc
  in List.fold_left f 0 l;;

count [1;2;3;4;5] 10;;
count [1;2;3;4;5] 3;;
count [1;3;2;3;4;3;5] 3;;

(* 1b *)
let make_palyndrome l =
  (List.fold_left (fun acc nex -> nex :: acc) [] l) @ l;;

make_palyndrome [1;2];;
make_palyndrome [1;2;3];;
make_palyndrome [];;

(* 2a *)
let fold_2 f b l =
  let fold_func (acc, idx) nex =
    (f acc nex idx), idx + 1 in
  let res, _ = List.fold_left fold_func (b, 0) l in res;;

let f acc nex idx = 
  "(f " ^ acc ^ " " ^ nex ^ " " ^ (string_of_int idx) ^ ")";;

fold_2 f "" ["a";"b";"c"];;

(* 2b *)
let rec ith l i d =
  let f acc nex idx =
    if idx = i then nex else acc in
  fold_2 f d l;;

ith ["a";"b";"c";"d"] 0 "";;
ith ["a";"b";"c";"d"] 1 "";;
ith ["a";"b";"c";"d"] 2 "";;
ith ["a";"b";"c";"d"] 3 "";;
ith ["a";"b";"c";"d"] 4 "";;

(* 3a *)
type 'a fun_tree =
  | Leaf of ('a -> 'a)
  | Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
  match t with
  | Leaf f -> f x
  | Node (l, r) -> apply_all r (apply_all l x);;

let f1 x = x + 1;;
let f2 x = x * 2;;
let f3 x = x + 3;;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t 0;;

(* 3b *)
let f1 = (+) 1;;
let f2 = (-) 2;;
let f3 = (+) 3;;
let t = Node(Node(Leaf f1, Leaf f2), Leaf f3);;
apply_all t 0;;

let f1 = (^) "a";;
let f2 x = x ^ "b";;
let f3 x = x ^ "ab";;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t "123";;

let f1 = List.fold_left (fun x y -> (y*2)::x) [];;
let f2 = List.fold_left (fun x y -> x@[y]) [];;
let t = Node(Node(Leaf f1, Leaf f1), Node(Leaf f1, Leaf f2));;
apply_all t [1;2;3];;

(* 3c *)
let rec compose t1 t2 =
  match (t1, t2) with
  | Leaf f1, Leaf f2 -> Leaf (fun x -> f1 (f2 x))
  | Node (l1, r1), Node (l2, r2) ->
      Node (compose l1 l2, compose r1 r2);;

let f1 = (^) "a";;
let f2 = (^) "b";;
let f3 = (^) "c";;
let t = Node(Node(Leaf f1, Leaf f2), Leaf f3);;
let c = compose t t;;
apply_all c "x";;
