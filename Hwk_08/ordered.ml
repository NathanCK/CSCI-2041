module type OrderedSig =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end


module Int : (OrderedSig with type t = int) = struct
type t = int
let eq = (=)
let lt = (<)
let leq = (<=)

end
