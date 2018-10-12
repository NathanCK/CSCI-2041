(* The full language, with type annontations for each identifier declaration. 

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

  | Id of string

  | App of expr * expr
  | If of expr * expr * expr

  (* These constructs now have type annontations *)
  | Let of string * typ * expr * expr
  | Lambda of string * typ * expr
  | LetRec of string * typ * expr * expr

and value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
  (* You may have added other variants to ``value`` *)

(* The "language of types" for this simple language. *)
and typ 
  = IntType
  | BoolType
  | FuncType of typ * typ 
  (* You may decide to add something here that is useful in error
     reporting, but is not an actual type of a value. *)

and environment = (string * value) list

type error =
  (* An unbound name error *)
  | UnboundName of string

  (* An incorrect type error.  The expr has a type (the second
     component) but one of the types in the ``typ list`` was
     expected. *)
  | IncorrectType of expr * typ * (typ list)

  | DivisionByZero of expr


type 'a result = OK of 'a
               | Err of error list

let rec lookup (n:string) (env: (string * 'a) list) : 'a result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> OK v
  | _ :: rest -> lookup n rest

type context = (string * typ) list


(* The main challenge of this problem is to complete ``eval`` and
   ``check`` in the same way that was done in the ``expr_let_typing.ml`` 
   file.
 *)

let rec eval (e:expr) (env: environment) : value result =
  match e with 
  | Val v -> OK v


let rec check (e:expr) (ctxt:context) : typ result =
  match e with 
  | Val (Int i) -> OK IntType
