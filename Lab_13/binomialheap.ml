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

module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list

  val empty : t
  val isEmpty : t -> bool
  val insert : elem -> t -> t
  val merge : t -> t -> t
  val findMin : t -> elem
  val deleteMin : t -> t
end

module BinomialHeap (Elem : OrderedSig) :
(BinomialHeapSig with type elem := Elem.t)  = struct

type elem = Elem.t
type tree = Node of int * elem * tree list
type t = tree list

let empty = []

let isEmpty (ts:t) = ts = empty

let rank (Node (r,x,c)) = r

let root (Node (r,x,c)) = x

let link (Node (r1,x1,c1)) (Node (r2,x2,c2)) : tree =
  if x1 <= x2
  then Node (r1+1, x1, (Node (r2,x2,c2))::c1 )
  else
  Node (r1+1, x2, (Node (r1,x1,c1))::c2)

let rec insTree t1 t2 =
  match t1,t2 with
  | t, ts -> (match ts with
             | [] -> [t]
             |ta ::tsa -> (if rank t < rank ta
                           then t :: ts
                           else insTree (link t  ta) tsa))

let insert x ts = insTree (Node (0, x, [])) ts

let rec merge trees1 trees2 =
  match trees1, trees2 with
  | ts1,ts2 -> (match ts1,ts2 with
               | _, [] -> ts1
               | [], _ -> ts2
               | t1::ts1a , t2::ts2a -> (if rank t1 < rank t2
                                        then t1 :: merge ts1a ts2
                                        else if rank t2 < rank t1
                                        then t2 :: merge ts1 ts2a
                                        else insTree (link t1 t2)
                                        (merge ts1a ts2a)))

let rec removeMinTree trees =
  match trees with
  | [] -> raise (Failure "EMPTY")
  |t::[] -> (t, [])
  | t:: ts -> (let treea = removeMinTree ts
              in
              (match treea with
              | (ta,tsa) -> if root t < root ta
                            then (t,ts)
                            else (ta,t::tsa)))

let findMin ts =
 let t = removeMinTree ts
 in match t with
    | (ta,_) -> root ta

let deleteMin ts =
  let t = removeMinTree ts
  in match t with
     | (Node (_,x,ts1),ts2) -> merge (List.rev ts1)  ts2







end


module BHI = BinomialHeap(Int)

let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5

let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6
