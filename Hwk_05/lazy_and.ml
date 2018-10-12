let rec ands l =
  match l with
  |[] -> true
  |x::xs when x -> ands xs
  |_ -> false

(* why when x equal to when x = true *)
