(*Kin Chan, HW1, CSCI 2041*)

(*Part 1*)
let even : int -> bool
  = fun x ->
    if x mod 2 = 0
    then true
    else false

let rec euclid : int -> int -> int
  = fun a b ->
  if a = b then a
  else if a < b then euclid a (b-a)
  else euclid (a-b) b

let frac_simplify : (int * int) -> (int * int)
  = fun (x,y) -> (x/(euclid x y),y/(euclid x y))

let rec max : int list -> int
  = fun x -> let bigger x1 x2 = if x1 > x2 then x1 else x2
  in
  match x with
  |[]-> raise (Failure "Input list must not be empty")
  |x1::[] -> x1
  |x1::x2::[] -> bigger x1 x2
  |x1::x2::rest -> max ((bigger x1 x2)::rest)

let rec take : int -> 'a list -> 'a list
  = fun x lis ->
  match lis with
  |[]->[]
  |x1::rest -> if x <= 0 then [] else x1::take (x-1) rest
