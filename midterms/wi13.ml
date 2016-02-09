(* 1a *)
let first f l =
  let base = None in
  let fold_fn acc elmt =
    match acc with
    | None when f elmt -> Some elmt
    | _ -> acc
  in List.fold_left fold_fn base l;;

let even x = (x mod 2 = 0);;
first even [1;3;4;5;7;9;11];;
first even [1;2;3;4;5;7;9;10;11];;
first even [1;3;5;7;9;11];;

(* 1b *)
let f a b c = (a = b + c);;
first ((=) 4) [1;2;3;4;5];;
first ((=) 10) [1;2;3;4;5];;
first ((<) 3) [1;2;3;4;5];;
first ((<) 10) [1;2;3;4;5];;
first ((<) 0) [1;2;3;4;5];;
first (f 1 2) [1;2;3;4;5];;
first (f 5 2) [1;2;3;4;5];;

(* 2a *)
let rec zip l1 l2 =
  match (l1, l2) with
  | [], l -> []
  | l, [] -> []
  | (h1 :: t1), (h2 :: t2) -> (h1, h2) :: (zip t1 t2);;

zip [1;2;3] [5;6;7];;
zip ['a';'b';'c'] [1;2;3];;
zip ['a'] [1;2;3];;
zip ['a';'b';'c'] [1;2];;

(* 2b *)
let map2 f l1 l2 =
  List.map (fun (a, b) -> f a b) (zip l1 l2);;

map2 (+) [1;2;3] [4;6;8];;
map2 (-) [1;2;3] [4;6;8];;
map2 (/) [10;9;4] [2;3;4];;
map2 (+) [1;2] [4;6;8];;

(* 2c *)
let map3 f l1 l2 l3 =
  List.map (fun (a, (b, c)) -> f a b c) (zip l1 (zip l2 l3));;

let add a b c = a + b + c;;
map3 add [1;2] [3;4] [5;6];;
map3 add [1] [3;4] [5;6];;

(* 3 *)
let rec unzip l =
  match l with
  | [] -> [], []
  | (h1, h2) :: t ->
      let t1, t2 = unzip t in
      (h1 :: t1), (h2 :: t2);;

unzip [(1,2);(3,4);(5,6)];;
unzip [('a',1);('b',2)];;
