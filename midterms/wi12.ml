(* 1a *)
let rec split l =
  let half = (List.length l) / 2 in
  let base = (0, [], []) in
  let fold_fn (i, l1, l2) elmt =
    let left = l1 @ (if i < half then [elmt] else []) in
    let right = l2 @ (if i >= half then [elmt] else []) in
    (i + 1, left, right) in
  let (_, l1, l2) = List.fold_left fold_fn base l in
  (l1, l2);;

split [23;1;8;3];;
split [23;1;8;3;6];;
split [23;1;8;3;6;20];;
split ["a";"b";"c"];;
split ["a"];;

(* 1b *)
let rec merge l1 l2 =
  match (l1, l2) with
  | ([], l) -> l
  | (l, []) -> l
  | (h1 :: t1, h2 :: t2) ->
      if h1 < h2
      then h1 :: (merge t1 l2)
      else h2 :: (merge l1 t2);;

merge [2;4;6;8] [1;3;5];;
merge [2;10;20] [1;2;3;4;5;8;10;12];;

(* 1c *)
let rec merge_sort l =
  match l with
  | [] -> l
  | [x] -> l
  | _ ->
    let left, right = split l in
    merge (merge_sort left) (merge_sort right);;

merge_sort [2;10;3;2;1];;
merge_sort [-10;0;10;-20;100;-100];;

(* 2 *)
(* http://www.csc.villanova.edu/~dmatusze/8310summer2001/assignments/ocaml-functions.html *)
let rec explode = function
  | "" -> []
  | s  -> (String.get s 0) ::
          explode (String.sub s 1 ((String.length s) - 1));;

let rec implode = function
  | []       -> ""
  | charlist -> (Char.escaped (List.hd charlist)) ^
                (implode (List.tl charlist));;

let replace s =
  let f c = if c = '-' then ' ' else c in
  implode (List.map f (explode s));;

replace "no hyphens";;
replace "one-hyphen";;
replace "some--more-hyphens";;
replace "";;
replace " ";;
replace "-";;

(* 3a *)
let app l x =
  List.map (fun f -> f x) l;;

let incr x = x + 1;;
let decr x = x - 1;;
app [incr;decr] 10;;

(* 3b *)
let [f1; f2] = app [(=); (<)] 2;;

f1 1;;
f1 2;;
f1 3;;
f2 1;;
f2 2;;
f2 3;;

(* 4 *)
(* All of the above, obviously *)
