(*Kin Chan, HW1, CSCI 2041*)
(*Edited by Rex Zhu,zhu00100*)
(*Part 2*)

(*Add new types*)

type int2d = (int * int)

(*give a clear type of x*)
let all_evens (x:int list)=
  List.filter (fun x1 -> x1 mod 2= 0) x

(*give a clear type of x*)
let increment_all (x:int list) =
  List.map (fun x1 -> x1 + 1) x

(*give a clear type of x*)
let max_fold (x:int list) =
  match x with
  |[] -> raise (Failure "Input list must not be empty")
  |x1::rest -> List.fold_left (fun x1 x2 -> if x1 > x2 then x1 else x2) x1 x

(* Add some simplification on the grammar and
show clear types of the input variables *)
let sum_prod (x:int list) : int2d =
  match x with
  |[]-> (0,1)
  |x1::rest ->((List.fold_left (fun x1 x2 -> x1 + x2) x1 rest),
  (List.fold_left (fun x1 x2 -> x1 * x2) x1 rest))

(*Add some segmentations and delete some redundant details*)
(* Add some simplification on the grammar and show clear types
of the input variables *)

let split (f: 'a -> bool) (lst : 'a list):'a list list =
  let (x2,x1) =
  List.fold_right
  (fun x (x2,x1) -> if f x then ((x1::x2),[]) else (x2, x::x1)) lst ([],[])
  in
  x1::x2
