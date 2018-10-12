(*Kin Chan, HW1, CSCI 2041*)
(*Edited by Rex Zhu, zhu00100*)

(*Part 3*)

(*created new type*)

type words = char list

type strLst = string list

type string2d = (string * string)

(*deploy new type :words*)
let read_file (file_name: string) : words =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

(*deploy new type:words*)
let implode (cs: words) : string =
  String.concat "" (List.map  (String.make 1) cs)

(*give clear types of input and simplify the grammar*)
let split (f: 'a -> bool) (lst : 'a list):'a list list =
  let (x2,x1) =
  List.fold_right
  (fun x (x2,x1) -> if f x then ((x1::x2),[]) else (x2, x::x1)) lst ([],[])
  in
  x1::x2

let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

let answers (x:string) : strLst =
  let read_word = read_file x
  in
  let splited = split (fun x -> x = ' ' || x = '\n') read_word
  in
  let drop_empty_string_6 =
  List.map implode (List.filter (fun x -> List.length x = 6) splited)
  in
  let drop_empty_string_4 =
  List.map implode (List.filter (fun x -> List.length x = 4) splited)
  in
  List.filter
  (fun x -> List.mem (String.sub x 1 4) drop_empty_string_4)
  drop_empty_string_6

(*deploy new type:string2d*)
let pretty_answers (x:strLst) : string2d list =
List.map (fun x1 -> (String.sub x1 1 4, x1)) x
