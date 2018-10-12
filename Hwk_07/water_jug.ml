
(* Kin (Nathan) Chan     chanx393@umn.edu 5330106
Write a comment near the top of your ``water_jug.ml`` file explaining
how you represent the state of the problem.  That is, what type do
you use to represent the amount of water in the jugs?
  
 *)
type left = 0 | 1 | 2 | 3 | 4
type right = 0 | 1 | 2 | 3
type state = left * right


type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug


let describe (four:int) (three:int) : string =
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

  let option l =
  match l with
  | [] -> None
  | sth -> Some sth

  let play () =
    let rec concat l =
      let count four three =
        match l with
        | [] -> []
        | hd::tl -> match hd with
                     | Fill4GallonJugFromTap ->
                     (if four = 0 || four = 1 || four = 2 || four = 3
                     then
                     (Fill4GallonJugFromTap, describe 4 three)::(concat tl))

                     | Fill3GallonJugFromTap ->
                     (if three = 0 || three = 1 || three = 2
                     then
                     (Fill4GallonJugFromTap, describe four 3)::concat tl)

                     | Empty4GallonJugOnGround ->
                     (if four = 1 || four = 2 || four = 3 ||four = 4
                     then
                     (Empty4GallonJugOnGround, describe 0 three)::concat tl)

                     | Empty3GallonJugOnGround ->
                     (if three = 1 || three = 2 || three = 3
                     then
                     (Empty3GallonJugOnGround, describe four 0)::concat tl)

                     | Fill4GallonJugFrom3GallonJug ->
                     (if three > 4 - four
                     then
                     (Fill4GallonJugFrom3GallonJug, describe 4 (three - 4 + four))::concat tl)

                     | Fill3GallonJugFrom4GallonJug ->
                     (if four > 3 - three
                     then
                     (Fill3GallonJugFrom4GallonJug, describe (four - 3 + three) 3)::concat tl)

                     | Empty4GallonJugInto3GallonJug ->
                     (if four + three <=3
                     then
                     (Fill3GallonJugFrom4GallonJug, describe 0 (three+four))::concat tl)
                     | Empty3GallonJugInto4GallonJug ->
                     (if three + four <=4
                     then
                     (Fill3GallonJugFrom4GallonJug, describe (three+four) 0)::concat tl)


      in 0 0
      in option count
