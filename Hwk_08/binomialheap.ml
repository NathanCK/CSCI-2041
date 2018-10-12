open Ordered

(*The property of binomial tree:
  1. A binomial tree with its degree(rank) will equal the number of childrens
     the root have. For example, a tree with rank 3, the root will have 3
     childrens.
  2. The binomial tree with k rank will have 2^k elements
  3. the tree has min heap property

  The property of binomial heap: a collection of binomial trees
  4. childrens's value must be smaller than their parents (min heap property)
  5. All the tree must have the min heap property
  6. Only one kind of degree of binomial tree can be in the heap
      for example, rank 1 tree is the only one rank 1 tree in the heap
  7. All trees in the heap are in sorted order. For example, (0,1,2,3,4,5)
  *)

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
  val findMinDirect: t -> elem
  val isBinomialTree: tree -> bool
  (* val dup : t -> bool
  val fla : t -> int list *)
  val isBinomialHeap: t -> bool
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
  | t::[] -> (t, [])
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


let rec findMinDirect ts =
  match ts with
  | [] -> raise (Failure "EMPTY")
  | [t] -> root t
  | t :: tss -> let min1 = root t
                in
                min min1 (findMinDirect tss)

(* let children = int_of_float ((float_of_int 2) ** (float_of_int num)) *)


(* The rankfun is a helper function for corrRank, and it will
  check if the rank is equal to the number of children in the list.
  Also, while check if the above is true, it will also check if there is
  2^k elements in the tree*)
let rec rankfun (Node (r,_,bts)) =
  r = List.length bts && List.for_all rankfun bts

(* The corrRank is to call the helper function for the layers in the tree.
   Also, it fulfills the properties 1, 2 ,and 3 from above description*)
let corrRank ts =
  match ts with
  | Node (_,_,lst) -> rankfun ts && List.for_all rankfun lst

(* The for_all is similar to List.for_all but it will take a elem from the
   root and do what exactly List.for_all does. *)
let rec for_all f i lst =
  match lst with
  | [] -> true
  | x::xs -> (f i x) && for_all f i xs

(* The minHeap is to find if the Node is in min heap property.*)
let rec minHeap num (Node (r,e,bts)) =
    num < e && for_all minHeap e bts

(* The allMin is to find if all the Node in the tree are in min heap
   property. It fulfills the property 4 and 5 from above description *)
let allMin ts =
  match ts with
  | Node (r,e,lst) -> for_all minHeap e lst

(* The isBinomialTree is just check if the tree fulfills the properties
   1,2,3,4.*)
let rec isBinomialTree ts = allMin ts && corrRank ts

(* The fal is just to convert the tree into int list with only taking the
   elem in the tree*)
let rec fla ts = List.map (fun (Node (r,_,_))-> r) ts

(* The helper is to find if there is any duplicate number in the int list *)
let rec helper lst =
  match lst with
  |[] -> false
  | x::xs -> List.mem x xs || helper xs

(* The dup is to find if the duplicate is existed by calling  the fla and
  helper. It fulfills the property 6 from above description*)
let rec dup ts =
  let a = fla ts
  in match helper a with
    | true -> false
    | false -> true

(* The helper1 is to find if the int list is in increasing order. *)
let rec helper1 lst =
  match lst with
  | [] -> true
  | x::[] -> true
  | x1::x2::rest -> if x1 < x2
                    then helper1 (x2::rest)
                    else false

(* The order is to create a int list from the heap and pass it to helper1
   to find if the int list is in increasing order. It fulfills the property
   7. *)
let rec order ts =
  let a = fla ts
  in match helper1 a with
  | true -> true
  | false -> false

(* The isBinomialHeap is to find a heap is binomial heap by calling dup,
   isBinomialTree with List.for_all, and order *)
let rec isBinomialHeap ts =
  match ts with
  | [] -> true
  | tree :: trees -> List.for_all isBinomialTree ts && dup ts && order ts


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
let m3 = BHI.findMinDirect h6
