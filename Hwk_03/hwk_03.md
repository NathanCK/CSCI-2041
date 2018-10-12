<!-- Kin Chan (chanx393)
     CSCI 2041 Homework 3 -->

Note: LHS = Left Hand Side
      RHS = Right Hand Side

      For example, x = y+z
                  LHS = x , RHS = y+z

<!-- Q1: -->
let rec power n x =
match n with
| 0 -> 1.0
| _ -> x *. power (n-1) x

To show: power n x = x^n for n

Induction principle:
∀n, P(n) if P(0) and P(n) => P(n+1)

Base case: n=0
power 0 x = x^0
power 0 x = 1             By the properties of power(math)
LHS: power 0 x
= 1.0                     By definition of power
= RHS

Inductive case: n = n+1
Given hypothesis: power n x = x^n
power (n+1) x = x^(n+1)
RHS: power (n+1) x
= x * (power (n+1)-1 x)   By definition of power
= x * (power n x)         By the properties of addition and subtraction
= x * (x^n)               By the given hypothesis
= x^(n+1)
= LHS
So, power n x = x^n is true for all n

<!-- Q2: -->
type nat = Zero | Succ of nat
let toInt = function
| Zero -> 0
| Succ n -> toInt n + 1
let rec power n x = match n with
| Zero -> 1.0
| Succ n’-> x *. power n’ x

To show: power n x = x^toInt(n) for n

Induction principle:
∀n ∈ nat, P(n) if p(Zero) and P(n) => P(Succ n)

Base case: n = Zero
power Zero x = x^toInt(Zero)
power Zero x = x^0          By the definition of toInt
power Zero x = 1            By the properties of power(math)
LHS: power Zero x
= 1.0                       By the definition of power
= RHS

Inductive case: n = Succ n
Given hypothesis: power n x = x^toInt(n)
power (Succ n) x = x^toInt(Succ n)
power (Succ n) x = x^toInt(n+1)     By the definition of toInt
LHS: power (Succ n) x
= x * (Power n x)                   By the definition of power
= x * x^toInt(n)                    By the given hypothesis
= x^toInt(n+1)                      By the properties of power and addition(math)
=RHS
So, power n x = x^toInt(n) is true for n

<!-- Q3: -->
let rec reverse l = match l with
| [] -> []
| (h::t) -> append (reverse t) [h]
let rec append l1 l2 = match l1 with
| [] -> l2
| (h::t) -> h :: (append t l2)

To show:
reverse (append l1 l2) = append (reverse l2) (reverse l1)

Induction principle:
∀l1 ∀l2, reverse (append l1 l2) = append (reverse l2) (reverse l1)

Base case: P([],l2)
reverse (append [] l2) = append (reverse l2) (reverse [])
LHS: reverse (append [] l2)
= reverse l2                              By the definition of append
RHS: append (reverse l2) (reverse [])
= append (reverse l2) []                  By the definition of reverse
= reverse l2                              By the definition of append
= LHS

Inductive case: P((x::xs), l2)
Given hypothesis: reverse (append l1 l2) = append (reverse l2) (reverse l1)
reverse (append (x::xs) l2) = append (reverse l2) (reverse (x::xs))
RHS: append (reverse l2) (reverse (x::xs))
= append (reverse l2) (append (reverse xs) [x])             By the definition of reverse
= append (reverse l2) (append (reverse xs) (reverse [x]))   By the definition of reverse (one element in a list has no effect of reversing)
= append (reverse l2) (reverse (append [x] xs))             By given hypothesis
= append (reverse l2) (reverse (x :: (append [] xs)))       By the definition of append
= append (reverse l2) (reverse (x :: xs))                   By the definition of append
= reverse (append (x :: xs) l2)                             By the given hypothesis
=LHS
So, reverse (append l1 l2) = append (reverse l2) (reverse l1) is true

<!-- Q4: -->
let isupper c = Char.code c >= Char.code ’A’ &&
Char.code c <= Char.code ’Z’
let rec someupper lst = match lst with
| [] -> false
| x::xs -> isupper x || someupper xs

To show: someupper (l1 @ l2) = someupper l1 || someupper l2

Induction principle:
∀l1 ∀l2, P(l1) if P([]) and P(l1) => P(l2==l1)

Base case: P([] , l2)
someupper ([]@l2) = someupper [] || someupper l2
LHS: someupper ([]@l2)
= isupper x || someupper []  By the definition of someupper
= isupper x || false         By the definition of someupper
RHS: someupper [] || someupper l2
= false || isupper x || someupper []  By the definition of someupper
= isupper x || false || false         By the definition of someupper
= isupper x || false                  By the definition of true table
= LHS

Inductive case: P(x::xs, l2)
Given hypothesis: someupper (l1@l2) = someupper l1 || someupper l2
someupper (x::xs@l2) = someupper (x::xs) || someupper l2
LHS: someupper (x::xs@l2)
= someupper (x::(xs@l2))      By understanding of lists and @
= isupper x || someupper (xs@l2) By the definition of someupper
= isupper x || someupper xs || someupper l2 By given hypothesis
RHS: someupper (x::xs) || someupper l2
= isupper x || someupper xs || someupper l2 By the definition of someupper
=LHS
So, someupper (l1 @ l2) = someupper l1 || someupper l2 is true

<!-- Q5: -->
let isupper c = Char.code c >= Char.code ’A’ &&
Char.code c <= Char.code ’Z’
let rec someupper lst = match lst with
| [] -> false
| x::xs -> isupper x || someupper xs
let rec foldr (f:’a -> ’b -> ’b) (l:’a list) (v:’b) : ’b =
match l with
| [] -> v
| x::xs -> f x (foldr f xs v)
let upperor c b = isupper c || b
let foldupper lst = foldr upperor lst false

To show: someupper chs = foldupper chs

Base case: chs = []
someupper [] = foldupper []
RHS: foldupper[]
= foldr upperor [] false        By the definition of foldupper
= false                         By the definition of foldr
LHS: someupper []
=false                          By the definition of someupper
=RHS

Inductive case: chs = (x::xs)
Given hypothesis: someupper chs = foldupper chs
LHS: someupper (x::xs)
= isupper x || someupper xs     By the definition of someupper
RHS: foldupper (x::xs)
= foldr upperor (x::xs) false   By the definition of foldupper
= upperor x (foldr upperor xs false)  By the definition of foldr
= isupper x || (foldr upperor xs false)  By the definition of upperor
= isupper x || foldupper xs       By the definition of foldupper
= isupper x || someupper xs       By given hypothesis
=LHS
So, someupper chs = foldupper chs is true

<!-- Q6: -->
type ’a tree = Leaf of ’a
| Branch of ’a tree * ’a tree
let min x y = if x < y then x else y
let rec mintree t = match t with
| Leaf v -> v
| Branch (t1, t2) -> min (mintree t1) (mintree t2)
let rec tfold (l:’a -> ’b) (f: ’b -> ’b -> ’b) (t: ’a tree) : ’b = match t with
| Leaf v -> l v
| Branch (t1, t2) -> f (tfold l f t1) (tfold l f t2)
let fold_mintree t = tfold (fun x -> x) min t

To show: mintree t = fold mintree t

Induction principle:
∀t, P(t) if P(Leaf v) and P(t1) and P(t2) => P(Branch (t1,t2))

Base case:  P(Leaf v)
mintree Leaf v = fold_mintree Leaf v
LHS: mintree Leaf v
= v         By the definition of mintree
RHS: fold_mintree Leaf v
= tfold (fun x -> x) min v         By the definition of fold_mintree
= (fun x -> x) v                   By the definition of tfold
= v                                By understanding of the lambda expression
= LHS

Inductive case: P(Branch(t1,t2))
Given hypothesis: mintree t = fold_mintree t
mintree Branch(t1,t2) = fold_mintree Branch(t1,t2)
LHS: mintree Branch(t1,t2)
= min (mintree t1) (mintree t2)   By the definition of mintree
    case 1: t1 < t2
          = t1        By the definition of min
    case 2: t1 > t2
          = t2        By the definition of min
RHS: fold_mintree Branch(t1,t2)
= tfold (fun x -> x) min Branch(t1,t2)                          By the definition of fold_mintree
= min (tfold (fun x -> x) min t1) (tfold (fun x -> x) min t2)   By the definition of tfold
= min (fold_mintree t1) (fold_mintree t2)                       By the definition of fold_mintree
= min (mintree t1) (mintree t2)                                 By given hypothesis
    case 1: t1 < t2
          = t1        By the definition of min
    case 2: t1 > t2
          = t2        By the definition of min
=LHS
So, mintree t = fold_mintree t is true
