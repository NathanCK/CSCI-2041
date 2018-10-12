open Intervals

module Int_comparable : (Comparable with type t = int) = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module Int_interval = Make_interval(Int_comparable)



(* The following line now works. *)
let i = Int_interval.create 3 4
