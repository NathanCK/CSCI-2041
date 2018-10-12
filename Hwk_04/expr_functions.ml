(* Adding function values, recursive let expressions, lambda
   expressions and function application.

   This language extends the language in ``expr_let.ml``.

   Eric Van Wyk
 *)

type expr
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

  | App of expr * expr
  | Lambda of string * expr

  | LetRec of string * expr * expr
  | If of expr * expr * expr

and value
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
  | Ref of expr

and environment = (string * value) list

let sumToN : expr =
    LetRec ("sumToN",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
   LetRec ("fact",
           Lambda ("n",
                   If (Eq (Id "n", Val (Int 0)),
                       Val (Int 1),
                       Mul (Id "n",
                            App (Id "fact",
                                 Sub (Id "n", Val (Int 1))
                                )
                           )
                      )
                  ),
           Id "fact"
          )


let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                 Lambda ("y", Add (Id "x", Id "y"))
                )
let inc' = App (add, Val (Int 1))

(* The add2 closure *)
let add2app =
  Let ("add2",
       Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
       App (Id "add2", Val (Int 4)))

let rec lookup (n: string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Name \"" ^ n ^ "\" not in scope"))
  | (n',v)::_ when n' = n -> v
  | _::rest -> lookup n rest


let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1,e2) -> freevars e1 @ freevars e2
  | Mul (e1,e2) -> freevars e1 @ freevars e2
  | Div (e1,e2) -> freevars e1 @ freevars e2
  | Lt (e1,e2) -> freevars e1 @ freevars e2
  | Eq (e1,e2) -> freevars e1 @ freevars e2
  | And (e1,e2) -> freevars e1 @ freevars e2
  | Not e1 -> freevars e1
  | Let (n, dexpr, body) ->
     freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
  | Id n -> [n]
  | App (e1,e2) -> freevars e1 @ freevars e2
  | Lambda (n, dexpr) -> (List.filter (fun n' -> n <> n') (freevars dexpr))
  | LetRec (n, dexpr, body) -> freevars body @ (List.filter (fun n' -> n <> n') (freevars dexpr))
  | If (i1,i2,i3) -> freevars i1 @ freevars i2 @ freevars i3


let rec eval (env: environment) (e: expr) : value =
  match e with
  | Val v -> v
  | Add (e1, e2) ->
    ( match (eval env e1, eval env e2) with
    | Int a1, Int a2 -> Int (a1 + a2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Sub (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Int s1, Int s2 -> Int (s1 - s2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Mul (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Int m1, Int m2 -> Int (m1 * m2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Div (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Int d1, Int d2 -> Int (d1 / d2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Lt (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Int l1, Int l2 -> Bool (l1 < l2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | And (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Bool a1, Bool a2 -> Bool (a1 && a2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Eq (e1,e2) ->
    ( match (eval env e1, eval env e2) with
    | Int eq1, Int eq2 -> Bool (eq1 = eq2)
    | Bool eq1, Bool eq2 -> Bool (eq1 = eq2)
    | _, _ -> raise (Failure ("IncorrectType"))
    )
  | Not e1 ->
    ( match (eval env e1) with
    | Bool true -> Bool false
    | Bool false -> Bool true
    | _ -> raise (Failure ("IncorrectType"))
    )
  | Let (n, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body
  | Id n -> lookup n env
  | Lambda (n, dexpr) -> Closure (n, dexpr, env)
  | LetRec (n, dexpr, body) -> eval ((n, eval ((n, Ref dexpr)::env) dexpr) :: env) body
  | If (e1,e2,e3) ->
    ( match eval env e1 with
    | Bool true -> eval env e2
    | Bool false -> eval env e3
    | _ -> raise (Failure ("IncorrectType"))
    )
  | App (e1,e2) ->
    ( match (eval env e1) with
      | Closure (n,dexpr,env1) -> eval ((n, eval env e2)::env1) dexpr
      | Ref ref1 -> eval env (App (ref1, e2))
      | _ -> raise (Failure ("IncorrectType"))
    )


let evaluate e = eval [] e

let rec serialize (e:expr) : string  =
  match e with
  | Val v ->
    ( match v with
      | Int v -> "Val (" ^ "Int " ^ string_of_int v ^ ")"
      | Bool v -> "Val (" ^ "Int " ^ string_of_bool v ^ ")"
      | _ -> raise (Failure ("Will only serialize integer and Boolean values"))
      (* | Closure (n, dexpr, env) ->
        ( match env with
          | [(x,y)] -> "Closure (\"" ^ n^"\", " ^ serialize dexpr ^ ", " ^ "[(\"" ^x^"\", " ^ string_of_int y ^")]"
        ) *)
    )

  | Add (a1,a2) -> "Add ("^serialize a1^", "^serialize a2^")"
  | Sub (s1,s2) -> "Sub ("^serialize s1^", "^serialize s2^")"
  | Mul (m1,m2) -> "Mul ("^serialize m1^", "^serialize m2^")"
  | Div (d1,d2) -> "Div ("^serialize d1^", "^serialize d2^")"

  | Lt (l1,l2) -> "Lt ("^serialize l1^", "^serialize l2^")"
  | Eq (e1,e2) -> "Eq ("^serialize e1^", "^serialize e2^")"
  | And (an1,an2) -> "And ("^serialize an1^", "^serialize an2^")"
  | Not n1 -> "Not ("^serialize n1^")"

  | Let (n, dexpr, body) ->
    "Let (\"" ^ n ^ "\", " ^ serialize dexpr ^ ", " ^ serialize body ^ ")"
  | Id n -> "Id \"" ^n^"\""

  | App (ap1,ap2) -> "App ("^serialize ap1^", "^serialize ap2^")"
  | Lambda (n, dexpr) ->
    "Lambda (\"" ^ n ^"\", " ^ serialize dexpr ^")"

  | LetRec (n, dexpr, body) ->
    "LetRec (\"" ^ n ^ "\", " ^ serialize dexpr ^ ", " ^ serialize body ^ ")"

  | If (i1,i2,i3) ->
    "If ("^serialize i1^", "^serialize i2^ ", " ^ serialize i3 ^")"


let rec unparse (e:expr) : string  =
  match e with
  | Val v ->
    ( match v with
      |Int v -> string_of_int v
      |Bool v -> string_of_bool v
      |_ -> raise (Failure "Will only unparse integer and Boolean values")
    )

  | Add (a1,a2) -> "(" ^ unparse a1 ^ " + " ^ unparse a2 ^")"
  | Sub (s1,s2) -> "(" ^ unparse s1 ^ " - " ^ unparse s2 ^")"
  | Mul (m1,m2) -> "(" ^ unparse m1 ^ " * " ^ unparse m2 ^")"
  | Div (d1,d2) -> "(" ^ unparse d1 ^ " / " ^ unparse d2 ^")"

  | Lt (l1,l2) -> "(" ^ unparse l1 ^ " < " ^ unparse l2 ^")"
  | Eq (e1,e2) -> "(" ^ unparse e1 ^ " = " ^ unparse e2 ^")"
  | And (an1,an2) -> "(" ^ unparse an1 ^ " & " ^ unparse an2 ^")"
  | Not n1 -> "(~" ^ unparse n1 ^")"

  | Let (n, dexpr, body) -> "(let "^n^" = "^unparse dexpr^" in "^unparse body ^")"
  | Id n -> n

  | App (ap1,ap2) -> "("^unparse ap1^" "^unparse ap2^")"
  | Lambda (n, dexpr) ->
    "(fun "^n^" -> " ^ unparse dexpr^")"

  | LetRec (n, dexpr, body) ->
    "(let "^n^" = "^unparse dexpr^" in "^unparse dexpr ^")"

  | If (i1,i2,i3) ->
    "(if "^unparse i1^" = true then "^unparse i2^", else "^unparse i3 ^")"

(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = evaluate e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = evaluate e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y",
              Val (Int 5),
              Let ("x",
                   Add (Id "y", Val (Int 5)),
                   Add (Id "x", Id "y")
                  )
             )

let () =
  assert (serialize e1 = "Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))");
  assert (serialize e6 =
            "Let (\"y\", Val (Int 5), Let (\"x\", " ^
              "Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))")


(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true)


(* increment *)
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )
let inc' = App (add, Val (Int 1))

(* The add2 closure *)
let add2app =
  Let ("add2",
       Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
       App (Id "add2", Val (Int 4)))

let () =
  assert (evaluate (App (inc, Val (Int 4))) = Int 5);
  assert (evaluate (Add (Val (Int 2), Val (Int 3))) = Int 5);
  assert (evaluate (App (inc', Val (Int 4))) = Int 5);
  assert (evaluate add2app = Int 6)


(* sumToN *)
let sumToN : expr =
    LetRec ("sumToN",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
    LetRec ("fact",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 1),
                        Mul (Id "n",
                             App (Id "fact",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "fact"
           )

(* Assert expressions to test our functions. *)
let () =
  assert (evaluate (App (sumToN, Val (Int 4))) = Int 10);
  assert (evaluate (App (sumToN, Val (Int 10))) = Int 55);
  assert (evaluate (App (sumToN, Val (Int 100))) = Int 5050);
  assert (evaluate (App (fact, Val (Int 0))) = Int 1);
  assert (evaluate (App (fact, Val (Int 1))) = Int 1);
  assert (evaluate (App (fact, Val (Int 2))) = Int 2);
  assert (evaluate (App (fact, Val (Int 4))) = Int 24)



(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
  print_endline ("Success! All tests passed.")
