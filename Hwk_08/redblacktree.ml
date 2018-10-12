open Ordered

(* Extra credit:
   Because when there is a red color node, then next children has to be
   black which means we go from the root to the leaf, we eliminate half
   of the element as being black which is log n which is equal to the
   height of the tree*)

(* The property of red-black tree:
   1. If the root is red, then its children can't be red (Only for red), but if
   the root is black, then its children can be black
   2. The tree has BST property
   *)
module type RedBlackSetSig = sig

  type elem
  type color = R | B
  type t = E | T of color * t * elem * t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool
  val isRedBlackTree: t -> bool
end

module RedBlackTree (Elem: OrderedSig) :
  (RedBlackSetSig with type elem := Elem.t) = struct

  type elem = Elem.t
  type color = R | B
  type t = E | T of color * t * elem * t

  let empty = E

  let rec member x ts =
    match ts with
    | E -> false
    | T (_, a, y,b) -> (if Elem.lt x y then member x a
                       else if Elem.lt y x then member x b
                       else true)

  let balance col t1 elem t2 =
    match (col,t1,elem,t2) with
    | (B,T (R,T(R,a,x,b),y,c),z,d)
    | (B,T (R,a,x,T (R,b,y,c)),z,d)
    | (B,a,x,T (R,T (R,b,y,c),z,d))
    | (B,a,x,T (R,b,y,T (R,c,z,d))) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | _ -> T (col,t1,elem,t2)

  let rec insert x ts =
    let rec ins ts' =
      (match ts' with
       | E -> T (R,E,x,E)
       | T (col,a,y,b)->(if Elem.lt x y then balance col (ins a) y b
                        else if Elem.lt y x then balance col a y (ins b)
                        else ts'))
      in match ins ts with
        | T (_,a,y,b) -> T (B,a,y,b)
        | E -> E

(* The helper is to get color of the element for the noColorViolate to
   use *)
let helper rbt =
  match rbt with
  | E -> B
  | T (col,a,y,b) -> col

(* The noColorViolate is to check if the tree violated the property of color.
   It fulfills the property 1 from above description*)
let rec noColorViolate rbt =
  match rbt with
  | E -> true
  | T (col,a,y,b) ->( if col = R && (helper a = R || helper b = R)
                     then false
                     else
                    noColorViolate a && noColorViolate b)

(* The helper1 is to find the value of the element for propertyBST to use *)
let helper1 rbt =
  match rbt with
  | E -> raise (Failure "Something went wrong!")
  | T (_,_,y,_) -> y

(* The propertyBST is to check if the tree maintains the BST property which
   means the left subtree is less than the root and the right subtree is
   greater than the root. It fulfills the property 2 from above description *)
let rec propertyBST rbt =
  match rbt with
  | E -> true
  | T (_,a,y,b) ->
    (match a, b with
     | E,E -> true
     | T (_) , T (_) -> (Elem.lt (helper1 a) y && (>) (helper1 b) y
       && propertyBST a && propertyBST b)
     | E, T (_) -> (>) (helper1 b) y && propertyBST b
     | T (_), E -> Elem.lt (helper1 a) y && propertyBST a)

(* The isRedBlackTree is just calling noColorViolate and propertyBST to
   find if the tree is red-black tree*)
let isRedBlackTree rbt = noColorViolate rbt && propertyBST rbt

end

module RBTI = RedBlackTree (Int)
