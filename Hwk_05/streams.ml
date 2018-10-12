(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee
let rec from n =
  print_endline ("step " ^ string_of_int n) ;
  Cons ( n,
         delay (fun () -> from (n+1) )
       )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

(* Exercise: Define a stream named ``ones`` of type int stream
   in which all values are ``1``. *)
let ones: int stream =
  let rec ones_h () =
    Cons (1, delay ones_h)
  in ones_h ()


(* write some common list processing functions *)

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd
     then Cons (hd, rest)
     else demand rest

let even x = x mod 2 = 0

let all_even = filter even nats

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let all_evens_v2 = map (fun x -> x * 2) nats

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

let all_evens_v3 = zip (+) nats nats

(* Computing factorials

   nats       = 1   2   3   4    5     6
                 \   \   \   \    \     \
                  *   *   *   *    *     *
                   \   \   \   \    \     \
   factorials = 1-*-1-*-2-*-6-*-24-*-120-*-720

   We can define factorials recursively.  Each element in the stream
   is the product of then "next" natural number and the previous
   factorial.
 *)

let factorials : int stream =
  let rec factorials_from n =
      Cons (n, delay (fun () -> zip ( * ) nats (factorials_from n)))
  in factorials_from 1


(* The code below is from Kin (Nathan) Chan *)

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

(* The function foldr is 'b type and it will two args as inputs,
   and the first arg is a fun which takes 'a and 'b lazee args as
   inputs and return a 'b result which is the type of the whole
   function foldr. the second arg is 'a stream type.
   What the foldr does is to do the pattern matching and if s is
   equal to Cons with hd and tl, it will use the first arg to determine
   the hd and tl which is called foldr again*)
let rec foldr (f: 'a -> 'b lazee -> 'b) (s: 'a stream) : 'b =
  match s with
  |Cons (hd, tl) -> f hd (delay (fun () -> foldr f (demand tl)))

let rec and_fold (s: bool stream) : bool =
  foldr (fun x y -> if x then (demand y) else false )  s
  (* match s with
  |Cons (true, tl) -> and_fold (demand tl)
  |Cons (false, tl ) -> false *)

let rec sum_positive_prefix (s: int stream) : int =
  let rec is_pos x y = if ((x > 0) && ((demand y) > 0 ))
                       then x + (demand y)
                       else x
  in
  foldr is_pos s

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


(* The code below is from Professor Eric Van Wyk. *)

let ns: int stream = zip ( - ) (from 1000) (cubes_from 1)
let ns2: int stream = zip ( - ) (from 1000) (cubes_from 2)
let are_positive ns = map (fun n -> n > 0) ns
let ns_positive : bool stream = are_positive ns
let primes = sieve (from 2)
(* The code below is from Kin (Nathan) Chan *)
