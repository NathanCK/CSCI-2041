open StreamModules

module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream

end



module Hwk5(S: StreamSig) : Hwk5Sig = struct
   (* add elements here to complete the functor *)
type 'a lazee = 'a hidden ref
  and 'a hidden = Value of 'a
                | Thunk of (unit -> 'a)
type 'a stream = Cons of 'a * 'a stream lazee

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
 | Value _ -> ()
 | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
 force l;
 match !l with
 | Value v -> v
 | Thunk f -> raise (Failure "this should not happen")

let rec take (n:int) (s : 'a stream) : ('a list) =
match n, s with
  | 0, _ -> []
  | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

let rec from n =
 print_endline ("step " ^ string_of_int n) ;
 Cons ( n, delay (fun () -> from (n+1) ))

let nats = from 1

let rec cubes_from (x: int) : int stream =
  Cons ((x*x*x) , delay (fun () -> cubes_from (x+1)))

let rec cubes_from_zip (x: int) : int stream =
  zip (fun x y -> x) (cubes_from x) (cubes_from x)

let rec cubes_from_map (x: int) : int stream =
  map (fun x -> x) (cubes_from x)

let rec drop (x: int) (s: 'a stream) : 'a stream =
  match x,s with
  |0,_ -> s
  |x, Cons (hd, tl) -> drop (x-1) (demand tl)

let rec drop_until (f: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  |Cons (hd, tl) -> match f hd with
                    |true -> Cons (hd, tl)
                    |_ -> drop_until f (demand tl)


let rec foldr (f: 'a -> 'b lazee -> 'b) (s: 'a stream) : 'b =
  match s with
  |Cons (hd, tl) -> f hd (delay (fun () -> foldr f (demand tl)))

let rec sum_positive_prefix (s: int stream) : int =
  let rec is_pos x y = if ((x > 0) && ((demand y) > 0 ))
                       then x + (demand y)
                       else x
  in
  foldr is_pos s

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd
     then Cons (hd, rest)
     else demand rest

let prime (x: int) : bool =
   let rec ind i =
     ((i * i) > (abs x)) || ((abs x) mod i <> 0 && ind (i+1))
   in
   (x <> 1) && (ind 2)

let rec sieve (s: int stream) : int stream =
  let sift (x: int) (s: int stream) =
    filter (fun x -> prime x) s
  in
  match s with
  | Cons (x, xs) -> Cons(x, delay (fun () -> sieve (sift x (demand xs))))

let primes = sieve (from 2)
end
