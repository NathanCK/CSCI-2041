
(*Modified by Rex Zhu(zhu00100@umn.edu)*)

(*For the whole functions, you can move out "fun" to make it clearer.
 You can change the position of "=" as well*)

let circle_circum_v1 : float -> float =
	fun r -> 2.0 *. 3.1415 *. r
(*correct*)

let circle_circum_v2 radius =
  let pi = 3.1415
  in
  pi *. 2.0 *. radius
(*correct*)

let rec product : int list -> int =
	fun xs ->
	match xs with
	| [] -> 1
	| x1::rest -> x1 * product rest
(*correct*)

let rec sum_sqrdiffs list =
        match list with
        | [] -> 0
        | y1 :: [] -> 0
        | y1 :: (y2 :: rest) -> (y1 - y2) * (y1 - y2) + sum_sqrdiffs (y2::rest)
(* insert a new condition for only one input number *)
(* simplify the expression *)

let distance : float * float -> float * float -> float =
	fun (x1,y1) (x2,y2) ->
	sqrt(((x2-.x1) *. (x2-.x1)) +. ((y2-.y1) *. (y2-.y1)))
(*correct*)

let upper_right (x,y) = x > 0.0 && y > 0.0
(*correct*)

let triangle_perimeter : float * float -> float * float -> float * float -> float =
	fun (x1,y1) (x2,y2) (x3,y3) ->
	(distance (x1,y1) (x2,y2)) +. (distance (x2,y2) (x3,y3)) +. (distance (x3,y3) (x1,y1))
(*correct*)
