open Intervals

module Rational_comparable : (Comparable with type t = (int * int)) = struct
  type t = (int * int)

  let helper e1 =
    match e1 with
    | (e11,e22) -> (float_of_int e11) /. (float_of_int e22)

  let compare e1 e2 =
    let e11 = helper e1
    in
    let e22 = helper e2
    in
    compare e11 e22

  let rec euclid = fun a b ->
    if a = b then a
    else if b = 0 then raise (Failure "dividing an integer by zero ")
    else if a < b then euclid a (b-a)
    else euclid (a-b) b

  let frac_simplify e1 =
    match e1 with
    | (_,0) -> raise (Failure "dividing an integer by zero ")
    | (0,_) -> (1,1)
    | (e11,e22) -> (e11/(euclid e11 e22),e22/(euclid e11 e22))

  let to_string e1 =
    match (frac_simplify e1) with
    | (e11,e22) -> (string_of_int e11) ^ "/" ^ (string_of_int e22)
end

module Rational_interval = Make_interval(Rational_comparable)



(* The following line now works. *)
let i = Rational_interval.create (3,0) (4, 5)

let () =
  print_endline ("The i is : " ^ Rational_interval.to_string i ^ "\n");


(* let rec euclid : int -> int -> int
  = fun a b ->
  if a = b then a
  else if a < b then euclid a (b-a)
  else euclid (a-b) b *)
