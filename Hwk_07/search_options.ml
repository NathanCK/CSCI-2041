(* NOTE: This file will be updated as the lectures cover more search
   techniques.  It is NOT COMPLETE as it now stands.

   Below are the functions developed in lecture for the unit on search
   as a programmin technique.  The slides are S7_Search.
 *)

(* Below, we generate all possible subsets of a set of values.
   Note that we are using lists to represent sets and simply
   assume that we do not have duplicates.  If this is a concern
   we could use a "dedup" function to remove duplicates.

   The important point here is to see that for each element in the
   list we have to make a choice - include it in the subset or do not
   include it.

   This leads to the two recursive calls, one that returns subsets
   with it, and one that returns subsets without it.

   See how the search tree we drew in class corresponds to the "call
   graph" of the functions?
 *)


(* The function below shows the same structure that we will see in the
   search functions below and in other files in this directory. *)
let gen_subsets lst
  = let rec helper partial_subset rest
      = match rest with
      | [] -> [ partial_subset ]
      | x::xs -> (helper (x :: partial_subset) xs)
                 @
                 (helper partial_subset xs)
    in helper [] lst


(* This function is fine for finding all subsets, but doesn't have the
   same structure as the following search functions. *)
let gen_subsets_alternate lst
= let rec helper set accum =
    match set with
    | [] -> accum
    | hd::tl -> helper tl (accum @ List.map (fun x -> hd::x) accum)
  in
  helper lst [ [] ]


(* ---
   Options
   ---
 *)

let s = [ 1; 3; -2; 5; -6 ]   (* sample set from the S7 slides *)

let sum lst = List.fold_left (+) 0 lst

(* Our first implementation of subsetsum uses options to indicate if
   we found a solution or not.  If our searching function 'try_subset'
   fails to find a value, it returns None; if it finds what we are
   looking for, then it returns that values wrapped up in a Some.
 *)

let subsetsum_option_v1 (lst : 'a list) : 'a list option
  = let rec helper partial_subset rest
      = if sum partial_subset = 0 && partial_subset <> [] && rest = []
        then Some partial_subset
        else
          match rest with
          | [] -> None
          | x::xs -> match helper (partial_subset@[x]) xs with
                     | Some result -> Some result
                     | None -> helper partial_subset xs
    in helper [] lst



(* Here is another implementation of the above algorithm using
   options.  The final value returned by the function is an int list,
   however.  The empty list indicating that no subset was found.

   The reason for writing this function is only to make it clear that
   using an option in the return type of the subsetsum function above
   was not related to our use of options in the recursive search
   procedure.
 *)

let subsetsum_option_v2 (lst: int list) : int list
  = let rec helper partial_subset rest
      = if sum partial_subset = 0 && partial_subset <> [] && rest = []
        then Some partial_subset
        else
          match rest with
          | [] -> None
          | x::xs -> match helper (x::partial_subset) xs with
                     | Some result -> Some result
                     | None -> helper partial_subset xs
    in match helper [] lst with
       | None -> []
       | Some lst -> lst





(* Below we see how we can keep searching once we've found a solution to
   the problem.
   It may be that this solution is not acceptable to the user or there
   is simply some other evaluation criteria that we with to apply to
   the found solution.
   This lets the program keep looking even after finding a solution.
 *)

(* First, a function for converting lists into strings *)
let show_list show l =
  let rec sl l =
    match l with
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"



(* This function processes a solution, letting the user decide if
   the solution is acceptable or not.

   If not, then we want to keep looking.  Thus, it returns None,
   indicating that we have not yet found a solution, at least not one
   that we want to keep.

   If it is acceptable, then Some s (the proposed solution) is returned.

   The function also takes a show function to print out the solution
   to the user.
 *)
let rec process_solution_option show s =
  print_endline ("Here is a solution: " ^ show s) ;
  print_endline ("Do you like it ?" ) ;
  match "Y" = String.sub (String.capitalize_ascii (read_line ())) 0 1 with
  | true  -> print_endline "Thanks for playing..." ; Some s
  | false -> None

(* This version of subsetsum will let the user choose from the
   discovered solutions, one at a time, until an acceptable one is
   found.

   The process_solution_optoin function returns None of a Some value
   to indicate that the search should continue or end.
 *)
let subsetsum_option (lst: int list) : int list option
  = let rec helper partial_subset rest
      = if sum partial_subset = 0 && partial_subset <> [] && rest = []
        then process_solution_option (show_list string_of_int) partial_subset
        else
          match rest with
          | [] -> None
          | x::xs -> match helper (x::partial_subset) xs with
                     | None -> helper partial_subset xs
                     | Some result -> Some result
    in helper [] lst
