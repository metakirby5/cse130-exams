(* 1a *)
let length l =
  List.fold_left (fun acc _ -> acc + 1) 0 l;;

length [1;2;3];;
length [];;

(* 1b *)
let remove l x =
  let f acc nex =
    if nex = x then acc else acc @ [nex] in
  List.fold_left f [] l;;

remove [1;2;3;2;3;4;5] 2;;
remove [1;2;3] 4;;
remove [] 5;;

(* 2a *)
let rec ith l i d =
  match l with
  | [] -> d
  | h :: t -> if i = 0 then h else ith t (i - 1) d;;

ith ["a";"b";"c";"d"] 0 "";;
ith ["a";"b";"c";"d"] 1 "";;
ith ["a";"b";"c";"d"] 2 "";;
ith ["a";"b";"c";"d"] 3 "";;
ith ["a";"b";"c";"d"] 4 "";;

(* 2b *)
let rec update l i n =
  match l with
  | [] -> []
  | h :: t -> (if i = 0 then n else h) :: (update t (i - 1) n);;

update ["a";"b";"c";"d"] 0 "ZZZ";;
update ["a";"b";"c";"d"] 1 "ZZZ";;
update ["a";"b";"c";"d"] 2 "ZZZ";;
update ["a";"b";"c";"d"] 3 "ZZZ";;
update ["a";"b";"c";"d"] 4 "ZZZ";;

(* 2c *)
let rec update2 l i n d =
  match l with
  | [] -> if i < 0 then []
  else (if i = 0 then n else d) :: (update2 l (i - 1) n d)
  | h :: t -> (if i = 0 then n else h) :: (update2 t (i - 1) n d);;

update2 ["a";"b";"c";"d"] 0 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 1 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 2 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 3 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 4 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 5 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 6 "ZZZ" "";;
update2 ["a";"b";"c";"d"] 7 "ZZZ" "";;

(* 3 *)
let categorize f l =
  let base = [] in
  let fold_fn acc elmt =
    let idx = f elmt in
    let bucket = ith acc idx [] in
    update2 acc idx (bucket @ [elmt]) []
  in List.fold_left fold_fn base l;;

let f i =
  if i < 0 then 0
  else if i < 10 then 1
  else if i < 20 then 2
  else 3;;

categorize f [1;2;-3;15;7;30;-1;22;33;14;105];;
categorize f [-3;12;14];;
categorize f [];;
