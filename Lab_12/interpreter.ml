(* Simple interpreter based on Denotational Semantcs.

   Eric Van Wyk
 *)

type value
  = Int of int
  | Bool of bool

type expr =
  | Val of value
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr
  | Not of expr
  | And of expr * expr
  | Mod of expr * expr

type environment = (string * value) list

let rec lookup name env =
  match env with
  | [ ] -> raise (Failure ("Name \"" ^ name ^ "\" not found."))
  | (k,v)::rest -> if name = k then v else lookup name rest

let rec eval (e: expr) (env: environment) : value =
  match e with
  | Val v -> v
  | Add (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "incompatible types, Sub")
    )
  | Mul (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 / v2)
       | _ -> raise (Failure "incompatible types, Div")
     )
  | Lt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _ -> raise (Failure "incompatible types, Lt")
     )
  | Eq (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool v1, Bool v2 -> Bool (v1 = v2)
       | _ -> raise (Failure "incompatible types, Eq")
     )
  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool v1, Bool v2 -> Bool (v1 && v2)
       | _ -> raise (Failure "incompatible types, And")
     )
  | Not e' ->
     ( match eval e' env with
       | Bool v -> Bool (not v)
       | _ -> raise (Failure "incompatible types, Not")
     )
  | Var n -> lookup n env
  | Mod (e1,e2) ->
     ( match eval e1 env, eval e2 env with
       | Int x, Int y -> Int (x mod y)
       | _ -> raise (Failure "incompatible types, Mod"))


let rec read_number () =
  print_endline "Enter an integer value:" ;
  try int_of_string (read_line ()) with
  | Failure _ -> read_number ()

let write_number n = print_endline (string_of_int n)


type state = environment

type stmt =
  | Assign of string * expr
  | Seq of stmt * stmt
  | ReadNum of string
  | WriteNum of expr
  | IfThen of expr * stmt
  | While of expr * stmt
  | IfThenElse of expr * stmt * stmt
  | Skip
  | For of string * expr * expr * stmt
  | Repeat of stmt * expr

let rec exec (s: stmt) (stt: state) : state =
  match s with
  | Assign (nm, e) ->  (nm, eval e stt) :: stt
  | Seq (s1, s2) -> exec s2 (exec s1 stt)
  | ReadNum nm -> let vi = read_number ()
                  in (nm, Int vi) :: stt
  | WriteNum e -> (match eval e stt with
                   | Int vi -> write_number vi; stt
                   | _ -> raise (Failure "Incompatible types on WriteNum")
                  )
  | IfThen (cond, s) -> (match eval cond stt with
                         | Bool false -> stt
                         | Bool true -> exec s stt
                         | _ -> raise (Failure "Incompatible types on IfThen")
                        )
  | While (cond, body) ->
      (match  eval cond stt with
      | Bool true ->  exec (While (cond, body)) (exec body stt)
      | Bool false -> stt
      | _ -> raise (Failure "Incompatible types on While"))
  | IfThenElse (expr, st1, st2) ->
      (match eval expr stt with
      | Bool true -> exec st1 stt
      | Bool false -> stt
      | _ -> raise (Failure "Incompatible types on IfThenElse")  )
  | Skip -> stt
  | For (str, expr1, expr2, body) ->
      (match eval expr1 stt, eval expr2 stt with
      | Int x, Int y when x <= y ->
        exec (For (str, expr1, Val (Int (y-1)), body))
          (exec body ((str, eval expr2 stt) :: stt))
      | Int x, Int y when x > y -> stt
      | _ -> raise (Failure "Incompatible types on For")  )

let program_sum_10_n =
(* n = 10
   sum = 0
   for i = 1 to n
     sum = sum + i
     n = sum
   write sum
   write n
 *)
  Seq (Assign ("n", Val (Int 10)),
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Var "n",
            Seq (Assign("sum", Add (Var "sum", Var "i")),
                 Assign("n", Var "sum")
                )
           ),
  Seq(WriteNum (Var "sum"),
      WriteNum (Var "n")
     ) ) ) )

let program_for =
  (* for i = 1 to 5
       write i
   *)
  For ("i", Val (Int 1), Val (Int 5), WriteNum (Var "i"))

let program_sum_10 =
(* sum = 0
   for i = 1 to 10
     sum = sum + i
   write sum
 *)
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Val (Int 10),
            Assign("sum", Add (Var "sum", Var "i"))),
       WriteNum (Var "sum")
      ) )

let num_sum = 11
let program_assign =
(* x := 1;
   y := x + 2;
 *)
  Seq (Assign ("x", Val (Int 1)),
       Assign ("y", Add (Var "x", Val (Int 2)))
      )

let program_seq =
(* x := 1;
   y := x + 2;
   z := y * 3;
   write z
 *)
  Seq (Assign ("x", Val (Int 1)),
  Seq (Assign ("y", Add (Var "x", Val (Int 2))),
  Seq (Assign ("z", Mul (Var "y", Val (Int 3))),
       WriteNum (Var "z")
      ) ) )
let program_ifthen_simple_1 =
(* y = 10
   if y < 15 then write y
 *)
  Seq (Assign ("y", Val (Int 10)),
       IfThen (Lt (Var "y", Val (Int 15)),
               WriteNum (Var "y"))
      )

let program_ifthen_simple_2 =
(* y = 0
   if y = 0 then
     y = y + 2
   if ! (y < 4) then
     y = y + 3
   if y < 10 then
     y = y + 4
 *)
  Seq (Assign ("y", Val (Int 0)),
  Seq (IfThen (Eq (Var "y", Val (Int 0)),
               Assign ("y", Add (Var "y", (Val (Int 2))))),
  Seq (IfThen (Not (Lt (Var "y", Val (Int 4))),
               Assign ("y", Add (Var "y", (Val (Int 3))))),
       IfThen (Lt (Var "y", Val (Int 10)),
               Assign ("y", Add (Var "y", (Val (Int 4)))))
     ) ) )





let program_while =
(* read x;
   i = 0;
   sum = 0;
   while (i < x) {
     write i;
     sum = sum + i;
     i = i + 1
   }
   write sum
 *)
  Seq (ReadNum "x",
  Seq (Assign ("i", Val (Int 0)),
  Seq (Assign ("sum", Val (Int 0)),
  Seq (While (Lt (Var "i", Var "x"),
	      Seq (WriteNum (Var "i"),
	      Seq (Assign ("sum", Add (Var "sum", Var "i")),
		   Assign ("i", Add (Var "i", Val (Int 1)))
	          ) ) ),
        WriteNum (Var "sum")
      ) ) ) )




let program_while_ifthenelse =
(* read x;
   i = 0;
   sum_evens = 0;
   sum_odds = 0;
   while (i < x) {
     write i;
     if i mod 2 = 0 then
        sum_evens = sum_evens + i;
     else
        sum_odds = sum_odds + i;
     i = i + 1
   }
   write sum_evens;
   write sum_odds
 *)
  Seq (ReadNum "x",
  Seq (Assign ("i", Val (Int 0)),
  Seq (Assign ("sum_evens", Val (Int 0)),
  Seq (Assign ("sum_odds", Val (Int 0)),
  Seq (While (
           Lt (Var "i", Var "x"),
	   Seq (WriteNum (Var "i"),
                Seq (IfThenElse (
                         Eq (Mod (Var "i", Val (Int 2)), Val (Int 0)),
                         Assign ("sum_evens", Add (Var "sum_evens", Var "i")),
                         Assign ("sum_odds", Add (Var "sum_odds", Var "i"))
                       ),
		     Assign ("i", Add (Var "i", Val (Int 1)))
         ) ) ),
       Seq (WriteNum (Var "sum_evens"),
            WriteNum (Var "sum_odds"))
      ) ) ) ) )



let program_while_ifthenelse_test =
  Seq (Assign ("x", Val (Int 12)),
  Seq (Assign ("i", Val (Int 0)),
  Seq (Assign ("sum_evens", Val (Int 0)),
  Seq (Assign ("sum_odds", Val (Int 0)),
  Seq (While (
           Lt (Var "i", Var "x"),
	   Seq (WriteNum (Var "i"),
                Seq (IfThenElse (
                         Eq (Mod (Var "i", Val (Int 2)), Val (Int 0)),
                         Assign ("sum_evens", Add (Var "sum_evens", Var "i")),
                         Assign ("sum_odds", Add (Var "sum_odds", Var "i"))
                       ),
		     Assign ("i", Add (Var "i", Val (Int 1)))
         ) ) ),
       Seq (WriteNum (Var "sum_evens"),
            WriteNum (Var "sum_odds"))
      ) ) ) ) )




let program_ifthen =
(* y = 0
   if x % 2 = 0 then
     y = y + 2
   if x % 3 = 0 then
     y = y + 3
   if x * 4 = 0 then
     y = y + 4
 *)
  Seq (Assign ("y", Val (Int 0)),
  Seq (IfThen (Eq (Mod (Var "x", Val (Int 2)), Val (Int 0)),
               Assign ("y", Add (Var "y", (Val (Int 2))))),
  Seq (IfThen (Eq (Mod (Var "x", Val (Int 3)), Val (Int 0)),
               Assign ("y", Add (Var "y", (Val (Int 3))))),
       IfThen (Eq (Mod (Var "x", Val (Int 4)), Val (Int 0)),
               Assign ("y", Add (Var "y", (Val (Int 4)))))
      ) ) )
