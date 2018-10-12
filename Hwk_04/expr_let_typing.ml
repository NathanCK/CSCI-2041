(* A definition of arithmetic, relational, logical, and let
   expressions and their evaluation.  This language was originally
   defined in ``expr_let.ml``.

   Here we change the type of ``eval`` and introduce static type
   checking of expressions.

   Eric Van Wyk
 *)

type value
  = Int of int
  | Bool of bool

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

type environment = (string * value) list

type typ =
  | IntType
  | BoolType

type error =
  (* An unbound name error *)
  | UnboundName of string

  (* An incorrect type error.  The expr has a type (the second
     component) but one of the types in the ``typ list`` was
     expected. *)
  | IncorrectType of expr * typ * (typ list)

  | DivisionByZero of expr

  let rec serialize (e:expr) : string  =
    match e with
    | Val v ->
      ( match v with
        | Int v -> "Val (" ^ "Int " ^ string_of_int v ^ ")"
        | Bool v -> "Val (" ^ "Int " ^ string_of_bool v ^ ")"
        (* | _ -> raise (Failure ("Will only serialize integer
        and Boolean values")) *)
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

    (* | App (ap1,ap2) -> "App ("^serialize ap1^", "^serialize ap2^")"
    | Lambda (n, dexpr) ->
      "Lambda (\"" ^ n ^"\", " ^ serialize dexpr ^")" *)
(*
    | LetRec (n, dexpr, body) ->
      "LetRec (\"" ^ n ^ "\", " ^ serialize dexpr ^ ", " ^
      serialize body ^ ")" *)

    (* | If (i1,i2,i3) ->
      "If ("^serialize i1^", "^serialize i2^ ", " ^ serialize i3 ^")" *)


  let rec unparse (e:expr) : string  =
    match e with
    | Val v ->
      ( match v with
        |Int v -> string_of_int v
        |Bool v -> string_of_bool v
        (* |_ -> raise (Failure "Will only unparse integer
        and Boolean values") *)
      )

    | Add (a1,a2) -> "(" ^ unparse a1 ^ " + " ^ unparse a2 ^")"
    | Sub (s1,s2) -> "(" ^ unparse s1 ^ " - " ^ unparse s2 ^")"
    | Mul (m1,m2) -> "(" ^ unparse m1 ^ " * " ^ unparse m2 ^")"
    | Div (d1,d2) -> "(" ^ unparse d1 ^ " / " ^ unparse d2 ^")"

    | Lt (l1,l2) -> "(" ^ unparse l1 ^ " < " ^ unparse l2 ^")"
    | Eq (e1,e2) -> "(" ^ unparse e1 ^ " = " ^ unparse e2 ^")"
    | And (an1,an2) -> "(" ^ unparse an1 ^ " & " ^ unparse an2 ^")"
    | Not n1 -> "(~" ^ unparse n1 ^")"

    | Let (n, dexpr, body) -> "(let "^n^" = "^unparse dexpr^"
      in "^unparse body ^")"
    | Id n -> n

    (* | App (ap1,ap2) -> "("^unparse ap1^" "^unparse ap2^")"
    | Lambda (n, dexpr) ->
      "(fun "^n^" -> " ^ unparse dexpr^")"

    | LetRec (n, dexpr, body) ->
      "(let "^n^" = "^unparse dexpr^" in "^unparse dexpr ^")" *)

    (* | If (i1,i2,i3) ->
      "(if "^unparse i1^" = true then "^unparse i2^", else "^unparse i3 ^")" *)



type 'a result = OK of 'a
               | Err of error list

let rec get_type v =
  match v with
  | Int v -> IntType
  | Bool v -> BoolType

let rec lookup (n:string) (env: (string * 'a) list) : 'a result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> OK v
  | _ :: rest -> lookup n rest


let rec eval (e:expr) (env: environment) : value result =
  match e with
  | Val v -> OK v
  | Add (e1, e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int a2 -> OK (Int (a1+a2))
    | OK Bool a1, _ -> Err ([ IncorrectType (e1, BoolType, [IntType])])
    | _, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Sub (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int a2 -> OK (Int (a1-a2))
    | OK Bool a1, _ -> Err ([ IncorrectType (e1, BoolType, [IntType])])
    | _, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Mul (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int a2 -> OK (Int (a1*a2))
    | OK Bool a1, _ -> Err ([ IncorrectType (e1, BoolType, [IntType])])
    | _, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Div (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int 0 -> Err ([ DivisionByZero e])
    | OK Bool a1, _ -> Err ([ IncorrectType (e1, BoolType, [IntType])])
    | _, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y
    | OK Int a1, OK Int a2 -> OK (Int (a1/a2))

    )
  | Lt (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int a2 -> OK (Bool (a1<a2))
    | OK Bool a1, _ ->  Err ([ IncorrectType (e1, BoolType, [IntType])])
    | _, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )

  | And (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Bool a1, OK Bool a2 -> OK (Bool (a1&&a2))
    | _, OK Int a2 -> Err ([ IncorrectType (e2, IntType, [BoolType])])
    | OK Int a1, _ -> Err ([ IncorrectType (e1, IntType, [BoolType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Eq (e1,e2) ->
    ( match (eval e1 env, eval e2 env) with
    | OK Int a1, OK Int a2 -> OK (Bool (a1=a2))
    | OK Bool a1, OK Bool a2 -> OK (Bool (a1=a2))
    | OK Bool a1, OK Int a2 -> Err ([ IncorrectType (e2, IntType, [BoolType])])
    | OK Int a1, OK Bool a2 -> Err ([ IncorrectType (e2, BoolType, [IntType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )

  | Not e1 ->
    ( match (eval e1 env) with
    | OK Bool true-> OK (Bool false)
    | OK Bool false -> OK (Bool true)
    | OK Int x -> Err ([ IncorrectType (e1, IntType, [BoolType])])
    | Err x -> Err x

    )
  | Id e1 -> lookup e1 env
  | Let (n, dexpr, body) ->
    ( match (eval dexpr env) with
      | OK x -> eval body ((n,x)::env)
      | Err x -> Err x


    )
      (* let v = eval dexpr env in
      eval body ( (n,v)::env ) *)




(* A helper function to start evaluation with the empty environment. *)
let evaluate e = eval e []


(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

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

(* Assert expressions to test the evaluate function. *)
let () =
  assert (evaluate e1 = OK (Int 7));
  assert (evaluate e2 = OK (Int 7));
  assert (evaluate e3 = OK (Bool true));
  assert (evaluate e4 = OK (Bool false));
  assert (evaluate e5 = OK (Bool true));
  assert (evaluate e6 = OK (Int 15));
  assert (evaluate e7 = OK (Bool true))


let er1 = Add (Val (Int 1), Mul (Val (Bool true), Val (Int 3)))
let er2 = Eq (Val (Bool true), Val (Int 3))
let er3 = Eq (e1, e4)

let er4 = Let ("y",
               Val (Int 5),
               And (Val (Bool true), Id "y")
              )

let er5 = And (Val (Bool true), Id "y")

let er6 = Let ("y",
               Val (Int 0),
               Div (Val (Int 5), Id "y")
              )

let er7 = Let ("x",
              Add (Val (Int 5), Val (Bool true)),
              Add (Id "x", Val (Int 5))
              )

let has_eval_errors (e:expr) : bool =
  match evaluate e with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (has_eval_errors er1);
  assert (has_eval_errors er2);
  assert (has_eval_errors er3);
  assert (has_eval_errors er4);
  assert (has_eval_errors er5);
  assert (has_eval_errors er6);
  assert (has_eval_errors er7)


(* To check the type correctness of expressions by infering their
   type, we use the following data types. *)

type context = (string * typ) list

let rec check (e:expr) (ctxt:context) : typ result =
  match e with
  | Val (Int _) -> OK IntType
  | Val (Bool _) -> OK BoolType
  | Add (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK IntType
    | OK BoolType, OK IntType ->
      Err ([ IncorrectType (a1, BoolType, [IntType])])
    | OK IntType, OK BoolType ->
      Err ([ IncorrectType (a2, BoolType, [IntType])])
    | OK BoolType, OK BoolType ->
      Err (IncorrectType (a1, BoolType, [IntType])::
        IncorrectType (a2, BoolType, [IntType])::[] )
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Sub (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK IntType
    | OK BoolType, _ -> Err ([ IncorrectType (a1, BoolType, [IntType])])
    | _, OK BoolType -> Err ([ IncorrectType (a2, BoolType, [IntType])])
    (* | OK BoolType, OK BoolType ->
      Err (IncorrectType (a1, BoolType, [IntType])::
        IncorrectType (a2, BoolType, [IntType])::[] ) *)
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Mul (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK IntType
    | OK BoolType, _ -> Err ([ IncorrectType (a1, BoolType, [IntType])])
    | _, OK BoolType -> Err ([ IncorrectType (a2, BoolType, [IntType])])
    (* | OK BoolType, OK BoolType ->
      Err (IncorrectType (a1, BoolType, [IntType])::
        IncorrectType (a2, BoolType, [IntType])::[] ) *)
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Div (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK IntType
    | OK BoolType, _ -> Err ([ IncorrectType (a1, BoolType, [IntType])])
    | _, OK BoolType -> Err ([ IncorrectType (a2, BoolType, [IntType])])
    (* | OK BoolType, OK BoolType ->
      Err (IncorrectType (a1, BoolType, [IntType])::
        IncorrectType (a2, BoolType, [IntType])::[] ) *)
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Lt (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK BoolType
    | OK BoolType, _ -> Err ([ IncorrectType (a1, BoolType, [IntType])])
    | _, OK BoolType -> Err ([ IncorrectType (a2, BoolType, [IntType])])
    (* | OK BoolType, OK BoolType ->
      Err (IncorrectType (a1, BoolType, [IntType])::
        IncorrectType (a2, BoolType, [IntType])::[] ) *)
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Eq (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK IntType, OK IntType -> OK BoolType
    | OK BoolType, OK BoolType -> OK BoolType
    | OK IntType, OK BoolType ->
      Err ([ IncorrectType (a2, BoolType, [IntType])])
    | OK BoolType, OK IntType ->
      Err ([ IncorrectType (a2, IntType, [BoolType])])
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | And (a1, a2) ->
    ( match (check a1 ctxt, check a2 ctxt) with
    | OK BoolType, OK BoolType -> OK BoolType
    | OK IntType, _ -> Err ([ IncorrectType (a1, IntType, [BoolType])])
    | _, OK IntType -> Err ([ IncorrectType (a2, IntType, [BoolType])])
    (* | OK IntType, OK IntType ->
      Err (IncorrectType (a1, IntType, [BoolType])::
        IncorrectType (a2, IntType, [BoolType])::[] ) *)
    | _, Err x -> Err x
    | Err y, _ -> Err y

    )
  | Not a1 ->
    ( match (check a1 ctxt) with
      | OK BoolType -> OK BoolType
      | OK IntType -> Err ([ IncorrectType (a1, IntType, [BoolType])])
      | Err x -> Err x
    )
  | Id a1 ->
    ( match (lookup a1 ctxt) with
      | Err x -> Err x
      | OK v -> OK v
    )
  | Let (n, dexpr, body) ->
    ( match (check dexpr ctxt) with
      | OK v -> check body ((n,v)::ctxt)
      | Err x -> Err x
    )

let e8 = Div (Val (Int 5), Val (Int 0))

let has_type_errors (e:expr) : bool =
  match check e [] with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (not (has_type_errors e8))

let () =
  assert (has_type_errors er1);
  assert (has_type_errors er2);
  assert (has_type_errors er3);
  assert (has_type_errors er4);
  assert (has_type_errors er5);
  (* er6 has not type error *)
  assert (has_type_errors er7)



let () =
  print_endline ("Success! All tests passed.")
