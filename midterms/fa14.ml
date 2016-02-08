(* 1a *)
type expr =
  | Const of int
  | Var of string
  | Op of string * expr * expr;;

let rec rename_var e n1 n2 =
  match e with
  | Const x -> e
  | Var v -> Var (if v = n1 then n2 else v)
  | Op (s, e1, e2) -> Op (s, (rename_var e1 n1 n2), (rename_var e2 n1 n2));;

rename_var (Op ("+", Var "a", Const 4)) "a" "b";;
rename_var (Op ("+", Var "a", Const 4)) "b" "c";;
rename_var (Op ("+", Op ("*", Var "x", Var "y"),
            Op ("-", Var "x", Var "z"))) "x" "y";;

(* 1b *)
let to_str e =
  let rec str_helper e top_level =
    match e with
    | Const x -> string_of_int x
    | Var v -> v
    | Op (s, e1, e2) ->
        let inner = (str_helper e1 false) ^ s ^ (str_helper e2 false) in
        if top_level then inner else "(" ^ inner ^ ")"
  in str_helper e true;;

to_str (Op ("+", Var "a", Const 4));;
to_str (Op ("+", Const 10, Op ("+", Const 10, Var "b")));;
to_str (Op ("+", Op ("*", Var "x", Var "y"), Op ("-", Var "x", Var "z")));;

(* 2 *)
let average_if f l =
  let folding_fn (sum, n) nex =
    if f nex then (sum + nex, n + 1) else (sum, n) in
  let base = (0, 0) in
  let sum, n = List.fold_left folding_fn base l in
  if n = 0 then 0 else sum / n;;

let even x = x mod 2 = 0;;
average_if even [1;2;3;4;5];;
average_if even [1;2;3;4;5;6;7;8];;
average_if even [1;3;5;7];;

(* 3a *)
let length_2 l =
  List.fold_left (+) 0 (List.map List.length l);;

length_2 [[1;2;3];[4;6]];;
length_2 [[1;2;3];[4;6];[9;10]];;
length_2 [[];[];[]];;

(* 3b *)
let length_3 l =
  List.fold_left (+) 0 (List.map length_2 l);;

length_3 [[[1;2;3]];[[4;6];[7;8]]];;
length_3 [[[1;2;3]];[[4;6];[7;8];[10;11]]];;

(* 4 *)
let f1 = List.map (fun x -> 2 * x);;
f1 [1;2;3;4];;

let f2 = List.fold_left (fun x y -> (y + 2) :: x) [];;
f2 [3;5;7;9];;

let f3 = List.fold_left (fun x y -> x @ [3 * y]) [];;
f3 [1;3;6];;

(* Not sure why OCaml won't let this be polymorphic. *)
let f = List.fold_left (fun x y -> y x);;
f 1 [(+) 1; (-) 2];;
let f = List.fold_left (fun x y -> y x);;
f "abc" [(^) "zzz"; (^) "yyy"];;
let f = List.fold_left (fun x y -> y x);;
f [1;2;3] [f1;f2;f3];;
