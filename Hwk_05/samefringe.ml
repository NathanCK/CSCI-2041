(* The code below is from Professor Eric Van Wyk. *)
type 'a bintree
  = Lf of 'a
  | Nd of 'a bintree * 'a bintree

let t1 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(3), Lf(4)))
and t2 = Nd(Lf(1), Nd(Lf(2), Nd(Lf(3), Lf(4))))
and t3 = Nd(Nd(Nd(Lf(1), Lf(2)), Lf(3)), Lf(4))
and t4 = Nd(Lf(4), Nd(Lf(3), Nd(Lf(2), Lf(1))))
and t5 = Nd(Nd(Nd(Lf(4), Lf(3)), Lf(2)), Lf(1))

(* Trees t1, t2, and t3 have the same fringe. *)
(* Trees t4 and t5 have the same fringe. *)

let rec append_strict l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: append_strict t l2

let rec equal_list_strict l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) -> x = y && equal_list_strict xs ys
  | _ -> false

let rec flatten_strict = function
  | Lf(x) -> [x]
  | Nd(l, r) -> append_strict (flatten_strict l) (flatten_strict r)

let eqleaves_strict t1 t2 =
  equal_list_strict (flatten_strict t1) (flatten_strict t2)

  type 'a lazee = 'a hidden ref
and 'a hidden
	= Value of 'a
	| Thunk of (unit -> 'a);;

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

  (* The code below is from Kin (Nathan) Chan *)


let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


type 'a lazy_list
  = Cons of 'a * 'a lazy_list lazee
  | Nil

let rec append_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : 'a lazy_list =
  match l1 with
  | Nil -> l2
  | Cons (x,y) -> Cons (x, delay (fun () -> append_lazy (demand y) l2))

let rec equal_list_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : bool =
  match (l1,l2) with
  | (Nil,Nil) -> true
  | (Cons (x1, x2), Cons (y1,y2)) ->
    x1=y1 && equal_list_lazy (demand x2) (demand y2)
  |_ -> false


let rec flatten_lazy (t: 'a bintree) : 'a lazy_list =
  match t with
  | Lf(x) -> Cons (x, delay (fun () ->Nil))
  | Nd(l, r) -> append_lazy (flatten_lazy l) (flatten_lazy r)

let eqleaves_lazy (t1: 'a bintree) (t2: 'a bintree) : bool =
  equal_list_lazy (flatten_lazy t1) (flatten_lazy t2)

(* eqleaves_lazy uses less space than eqleaves_strict because when we do
   the flatten_lazy to a bintree to convert it to lazy_list, it will just 
   store a lazee expression instead of a whole list, and then when we need to
   use it, then it become a value.*)

(* let t6 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(5), Lf(4))) *)

(*
let a1 = flatten_lazy t1
and a2 = flatten_lazy t2
and a3 = flatten_lazy t3
and a4 = flatten_lazy t4
and a5 = flatten_lazy t5
and a6 = flatten_lazy t6 *)
(* let rec take (n:int) (s : 'a lazy_list) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl) *)
