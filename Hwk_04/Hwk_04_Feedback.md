## Feedback for Homework 04

Run on March 24, 13:08:49 PM.

+ Pass: Change into directory "Hwk_04".

## Feedback for part 1

+ Pass: Check that file "expr_functions.ml" exists.

+ Pass: Check that an OCaml file "expr_functions.ml" has no syntax or type errors.

    OCaml file "expr_functions.ml" has no syntax or type errors.



Please note that whitespace in not considered in the test for ```serialize``` and ```unparse```.

+ Pass: 
Check that the result of evaluating
   ```
   serialize ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `"Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   serialize ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `"Let (\"y\", Val (Int 5), Let (\"x\", Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   serialize ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `"Lambda (\"n\", Add (Id \"n\", Val (Int 1)))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   unparse ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `"(1 + (2 * 3))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   unparse ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `"(let y = 5 in (let x = (y + 5) in (x + y)))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   unparse ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `"(fun n -> (n + 1))"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   unparse (App (( Lambda ("n", Add (Id "n", Val (Int 1))) ), Val (Int 4)))
   ```
   matches the pattern `"((fun n -> (n + 1)) 4)"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   freevars ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   freevars ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   freevars ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   freevars (Id "x")
   ```
   matches the pattern `["x"]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   freevars (Lambda ("x", Id "y"))
   ```
   matches the pattern `["y"]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e1
   ```
   matches the pattern `Int 7`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e2
   ```
   matches the pattern `Int 7`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e3
   ```
   matches the pattern `Bool true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e4
   ```
   matches the pattern `Bool false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e5
   ```
   matches the pattern `Bool true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e6
   ```
   matches the pattern `Int 15`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e7
   ```
   matches the pattern `Bool true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (inc, Val (Int 4)))
   ```
   matches the pattern `Int 5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (Add (Val (Int 2), Val (Int 3)))
   ```
   matches the pattern `Int 5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (inc', Val (Int 4)))
   ```
   matches the pattern `Int 5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate add2app
   ```
   matches the pattern `Int 6`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 4)))
   ```
   matches the pattern `Int 10`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 10)))
   ```
   matches the pattern `Int 55`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 100)))
   ```
   matches the pattern `Int 5050`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 0)))
   ```
   matches the pattern `Int 1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 1)))
   ```
   matches the pattern `Int 1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 2)))
   ```
   matches the pattern `Int 2`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 4)))
   ```
   matches the pattern `Int 24`.

   




## Feedback for part 2

+ Pass: Check that file "expr_let_typing.ml" exists.

+ Pass: Check that an OCaml file "expr_let_typing.ml" has no syntax or type errors.

    OCaml file "expr_let_typing.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   evaluate e1
   ```
   matches the pattern `OK (Int 7)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e2
   ```
   matches the pattern `OK (Int 7)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e3
   ```
   matches the pattern `OK (Bool true)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e4
   ```
   matches the pattern `OK (Bool false)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e5
   ```
   matches the pattern `OK (Bool true)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e6
   ```
   matches the pattern `OK (Int 15)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate e7
   ```
   matches the pattern `OK (Bool true)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate er1
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate er2
   ```
   matches the pattern `Err [IncorrectType (Val (Int 3), IntType, [BoolType])]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate er5
   ```
   matches the pattern `Err [UnboundName "y"]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate er6
   ```
   matches the pattern `Err [DivisionByZero (Div (Val (Int 5), Id "y"))]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   evaluate (Add (Val (Bool true), Val (Bool false)))
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   check e8 []
   ```
   matches the pattern `OK IntType`.

   




+ Pass: 
Check that the result of evaluating
   ```
   check er1 []
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   check er2 []
   ```
   matches the pattern `Err [IncorrectType (Val (Int 3), IntType, [BoolType])]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   check er5 []
   ```
   matches the pattern `Err [UnboundName "y"]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   check (Add (Val (Bool true), Val (Bool false))) []
   ```
   matches the pattern `Err
[IncorrectType (Val (Bool true), BoolType, [IntType]);
IncorrectType (Val (Bool false), BoolType, [IntType])]`.

   




## Extlint

Various code style and organization checks are run on your code to detect common errors.

A description of the checks can be found here:  https://github.umn.edu/umn-csci-2041-S18/public-class-repo/blob/master/Course%20Info/extlint.md

### expr_functions.ml

## Extlint

+ Pass: Check that file "expr_functions.ml" exists.

+ Pass: **All extlint tests passed!**

### expr_let_typing.ml

## Extlint

+ Pass: Check that file "expr_let_typing.ml" exists.

+ Pass: **All extlint tests passed!**

