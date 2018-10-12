(*Kin Chan, HW1, CSCI 2041*)

(*Part 2*)
let all_evens : int list -> int list
  = fun x ->
  List.filter (fun x1 -> x1 mod 2= 0) x

let increment_all: int list -> int list
  = fun x ->
  List.map (fun x1 -> x1 + 1) x

let max_fold : int list -> int
  = fun x ->
  match x with
  |[] -> raise (Failure "Input list must not be empty")
  |x1::rest -> List.fold_left (fun x1 x2 -> if x1 > x2 then x1 else x2) x1 x

let sum_prod : int list -> int * int
  = fun x ->
  match x with
  |[]-> (0,1)
  |x1::rest ->((List.fold_left (fun x1 x2 -> x1 + x2) x1 rest),(List.fold_left (fun x1 x2 -> x1 * x2) x1 rest))

let split : ('a -> bool) -> 'a list -> 'a list list
  =fun f lst ->
  let (x2,x1) = List.fold_right (fun x (x2,x1) -> if f x then ((x1::x2),[]) else (x2, x::x1)) lst ([],[])
  in
  x1::x2
