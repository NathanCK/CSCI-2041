(*Kin Chan, HW1, CSCI 2041*)

(*Part 4*)
let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let split : ('a -> bool) -> 'a list -> 'a list list
  =fun f lst ->
  let (x2,x1) = List.fold_right (fun x (x2,x1) -> if f x then ((x1::x2),[]) else (x2, x::x1)) lst ([],[])
  in
  x1::x2

let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

    let group_by_3 lst =
    let accum = ( [], [], 0)
    in
    let f (sublists, current, size) x =
      if size = 3
      then ( List.rev current :: sublists , [x], 1 )
      else ( sublists, x :: current, size + 1 )
    in
    let (lsts, curr, size) = List.fold_left f accum lst
    in
    List.rev (List.rev curr :: lsts)

let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "

let format : string -> int -> string =
  fun str size ->
  let stage_1 = explode str
  in
  let stage_2 = List.filter (fun x -> x != []) (split (fun x -> x = ' ' || x =  '\t' || x =  '\n' || x =  '\r') stage_1)
  in
  let stage_3 = List.map implode stage_2
  in
  let accum = ("" , size)
  in
  let f (current, si) x =
    if si = size
    then (current^x, si - (String.length x))
    else if si - (String.length x) -1 >= 0
    then (current^" "^x, si - (String.length x) -1)
    else
     (current^"\n"^x,size-(String.length x))
     in
     let (cur, sizes) =  List.fold_left f accum stage_3
     in
     cur

  (* List.fold_left (fun x a -> if (String.length (x^" "^a)<= size) then (x^" "^a) else (x^"\n")) "" stage_3 *)


(*  let sum = List.fold_left (fun a x -> x + a) 0
let concat = List.fold_left (fun a x -> a ^ x) "" *)
