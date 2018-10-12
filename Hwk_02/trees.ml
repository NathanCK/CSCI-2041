(* Kin Nathan Chan
 * Homework 2
 *)

(* Part A *)
type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree

let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))

let rec t_size (x:'a tree) : int =
  match x with
  |Leaf _ -> 1
  |Fork (y,y1,y2) -> 1 + t_size y1 + t_size y2

let rec t_sum (x: int tree) : int =
  match x with
  |Leaf x1 -> x1
  |Fork (y,y1,y2) -> y + t_sum y1 + t_sum y2

let rec t_charcount (x: string tree) : int =
  match x with
  |Leaf x1 -> String.length(x1);
  |Fork (y,y1,y2) -> String.length(y) + t_charcount y1 + t_charcount y2

let rec t_concat (x: string tree) : string =
  match x with
  |Leaf x1 -> x1
  |Fork (y,y1,y2) -> y ^ t_concat y1 ^ t_concat y2


(* Part B *)
let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))

let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None,
                                                          Leaf (Some "d")))
let t100 = Fork (None, Leaf (Some "s"), Fork (None, Leaf None, Leaf (
  Some "b")))

let rec t_opt_size (x: 'a option tree) : int =
  match x with
  |Leaf None -> 0
  |Leaf (Some _) -> 1
  |Fork (Some _, y1 ,y2) -> 1 + t_opt_size y1 + t_opt_size y2
  |Fork (None, y1,y2) -> t_opt_size y1 + t_opt_size y2

let rec t_opt_sum (x: int option tree) : int =
  match x with
  |Leaf None -> 0
  |Leaf (Some x1) -> x1
  |Fork (Some y, y1,y2) -> y + t_opt_sum y1 + t_opt_sum y2
  |Fork (None, y1,y2) -> t_opt_sum y1 + t_opt_sum y2

let rec t_opt_charcount (x: string option tree) : int =
  match x with
  |Leaf None -> 0
  |Leaf (Some x1) -> String.length(x1)
  |Fork (Some y, y1,y2) -> String.length(y) +
                  t_opt_charcount y1 + t_opt_charcount y2
  |Fork (None, y1,y2) -> t_opt_charcount y1 + t_opt_charcount y2

let rec t_opt_concat (x: string option tree) : string =
  match x with
  |Leaf None -> ""
  |Leaf (Some x1) -> x1
  |Fork (Some y,y1,y2) -> y ^ t_opt_concat y1 ^ t_opt_concat y2
  |Fork (None,y1,y2) -> t_opt_concat y1 ^ t_opt_concat y2

(* Part C *)
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b =
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size (x: 'a tree) : int =
  tfold (fun x1 -> 1) (fun y y1 y2 -> 1 + y1 + y2) x

let tf_sum (x: int tree) : int =
  tfold (fun x1 -> x1) (fun y y1 y2 -> y + y1 + y2) x

let tf_charcount (x: string tree) : int =
  tfold (fun x1 -> String.length(x1)) (fun y y1 y2 -> String.length(y)
                          +   y1 +   y2) x
let tf_concat (x: string tree) : string =
  tfold (fun x1 -> x1) (fun y y1 y2 -> y ^ y1 ^ y2) x

let tf_opt_size (x: 'a option tree) : int =
  let helper x1 = match x1 with
                  |None -> 0
                  |Some _-> 1
in tfold helper (fun y y1 y2 -> helper y + y1 +y2) x

let tf_opt_sum (x: int option tree) : int =
  let helper x1 = match x1 with
                  |None -> 0
                  |Some x2 -> x2
in tfold helper (fun y y1 y2 -> helper y + y1 + y2) x

let tf_opt_charcount (x: string option tree) : int =
  let helper x1 = match x1 with
                  |None -> 0
                  |Some x2 -> String.length(x2)
in tfold helper (fun y y1 y2 -> helper  y + y1 + y2 ) x

let tf_opt_concat (x: string option tree) : string =
  let helper x1 = match x1 with
                  |None -> ""
                  |Some x2 -> x2
in tfold helper (fun y y1 y2 -> helper y ^ y1 ^y2) x

(* Part D *)
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

let rec bt_insert_by (f: 'a -> 'a -> int) (x: 'a) (y: 'a btree) : 'a btree =
  match y with
  |Empty -> Node (Empty, x, Empty)
  |Node (z, z1, z2) -> if (f x z1) > 0 then Node (z, z1, bt_insert_by f x z2)
                       else Node (bt_insert_by f x z, z1, z2)

let rec bt_elem_by (f: 'a -> 'b -> bool) (x: 'b) (y: 'a btree) : bool =
  match y with
  |Empty -> false
  |Node (z, z1, z2) ->  bt_elem_by f x z || f z1 x || bt_elem_by f x z2

let rec bt_to_list (x: 'a btree) : 'a list =
  match x with
  |Empty -> []
  |Node (y, y1, y2) -> List.append (bt_to_list y)  (y1 :: bt_to_list y2)

let rec btfold (x: 'b) (f: 'b -> 'a -> 'b -> 'b) (y:'a btree) : 'b =
  match y with
  |Empty -> x
  |Node (z, z1, z2) -> f (btfold x f z) z1 (btfold x f z2)

let btf_elem_by (f: 'a -> 'b -> bool) (x: 'b) (y: 'a btree) : bool =
  match y with
  |Empty -> false
  |Node _ -> btfold true (fun q q1 q2 -> q || (f q1 x) || q2) y

let btf_to_list (x: 'a btree) : 'a list =
  match x with
  |Empty -> []
  |Node _ -> btfold [] ( fun q q1 q2 -> List.append (q) (q1::q2)) x

(* using ``btfold`` for creating a function like
``bt_insert_by`` might be difficult because first,
  while inserting the elem, we go through the tree
  from top to bottom, but the btfold starts from the bottom
  of the tree (children) to the top (root). If we
  would like to insert elem using fold, we may use a fold
  that is go from top to bottom.  
 *)
