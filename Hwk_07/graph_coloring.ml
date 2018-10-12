(* Kin (Nathan) Chan     chanx393@umn.edu 5330106
1. What is the search space that your solution explores?  Specifically,
   explain what decisions are made at each step in exploring this
   space.  In the case of the subset-sum problem, we made a decision
   about including, or not including, an element from the original set in
   the partial subset we were constructing.  Explain the decisions being
   made in your solution, with references to those parts of your
   solution.

   My solution uses O(n^2) because my two helper - allThere and allPass
    will go throgh the whole list to keep track if they fullfill the
    requirment. The allThere would take node list and the result and check
    that if all the nodes are put into the result list, if yes then return
    true, otherwise return false. The allPass would take edge list and the
    result list, and check if all the neighborhoods are in the different
    color. In the pattern matching cases after the first if else
    , all those opterations are just defined differently for different
    condition uses. if they don't for this option, then will skip this option
    and do the next option instead of continuing on this option. Including
    is being made in my solution.Note that the result doesn't mean the full
    answer, it may just have only few of the node inside.

2. In exploring the potential coloring of a graph, your solution must
   not continue searching along that path if two adjacent nodes are
   colored with the same color.  Explain how your solution avoids this
   potential inefficiency.

   Note that we did not have this concern in the subset-sum problem.
   In choosing certain elements of the set, we could not tell if this
   was a dead end because the last element in the set could result in
   getting a sum of 0.  In this problem, that is not the case.  If we
   color adjacent nodes with the same color early in the process then
   there is no point in continuing.

   I used my two helper - allThere and allPass to help the function return
   a correct solution once it finds one and stop the case if we find two
   neighborhoods are in the same color since the allPass will take the
   edge list and the result and determine if there neighborhoods are in the
   same color, Note that the result doesn't mean the full answer, it may just
   have only few of the node inside.

   *)




type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

let g1 = ( [N 1; N 2; N 3; N 4],
              [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )
let g456 = [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4)]
let g123 = [N 1; N 2; N 3; N 4]
let g2 = ([N 1;N 2;N 3;N 4;N 5;N 6],
            [(N 1,N 1);(N 2,N 2);(N 3,N 3);(N 4,N 4);(N 5,N 5);(N 6,N 6)])
let g3 = ([N 1;N 2;N 3;N 4;N 5;N 6],[])
let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]

let existsL lst value = List.exists (fun (x, _) -> x = value) lst

let exists_value lst value = try List.assoc value lst with
                            |Not_found -> (C 10000000)

let rec allThere result nodelst =
  match nodelst with
  | [] -> true
  | x::xs -> existsL result x && allThere result xs

let rec allPass result edgelst =
  match edgelst with
  |[] -> true
  |(x1,x2)::tl -> match (exists_value result x1, exists_value result x2) with
                  | (z1,z2) -> if z1 = C 10000000 || z2 = C 10000000
                               then false
                               else not(z1=z2) && allPass result tl

let color_option (grh: graph) : coloring option
  = match grh with
    |(nodelst, edgelst) ->
      let rec helper num result nodelst edgelst
       = if ((allThere result nodelst)
             && (allPass result edgelst) && result <> [])
         then Some result
         else
         match nodelst with
         |[] -> None
         | x::xs -> if num = 1
         then (match helper (num+1) (result@[(x,C num)]) xs edgelst with
         | Some results -> Some results
         | None -> helper num result xs edgelst)
         else if num = 2 then
          (match helper (num+1) (result@[(x,C num)]) xs edgelst with
          | Some results -> Some results
          | None -> helper num result xs edgelst)
         else (match helper (num-2) (result@[(x,C num)]) xs edgelst,
                      helper (num-1) (result@[(x,C num)]) xs edgelst with
              | Some results,_ -> Some results
              | None, Some results -> Some results
              | None, _-> helper num result xs edgelst)
     in helper 1 [] nodelst edgelst

exception FoundColoring of coloring

let color_exception (grh: graph) : unit
= match grh with
  |(nodelst, edgelst) ->
    let rec helper num result nodelst edgelst
    = if num <= 3
      then if (allThere result nodelst)
            && (allPass result edgelst) && result <> []
           then raise (FoundColoring result)
           else
           match nodelst with
           |[] -> ()
           | x1::xs -> if num >= 3
                      then (helper (num-2) (result@[(x1, C num)]) xs edgelst;
                      helper (num-1) (result@[(x1, C num)]) xs edgelst)
                      else if num = 2
                           then
                           (helper (num+1) (result@[(x1, C num)]) xs edgelst;
                           helper num result xs edgelst)
                      else if num = 1
                           then
                           (helper (num+1) (result@[(x1, C num)]) xs edgelst;
                           helper num result xs edgelst)
     in helper 1 [] nodelst edgelst
