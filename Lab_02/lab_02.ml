let circle_circum_v1 : float -> float
	= fun r -> 2.0 *. 3.1415 *. r

let circle_circum_v2 : float -> float
	= fun r -> let pi = 3.1415 in 2.0 *. pi *. r

let rec product : int list -> int
	= fun xs ->
	match xs with
	| [] -> 0
	| x::[] -> x
	| x1::(x2::rest) -> x1 * x2 * product rest

let rec sum_sqrdiffs : int list -> int
	= fun xs ->
	match xs with
	| x1::(x2::[]) -> (x1 - x2) * (x1 - x2)
	| y1::(y2::rest) -> sum_sqrdiffs (y1::y2::[]) + sum_sqrdiffs (y2::rest)

let distance : float * float -> float * float -> float =
	fun (x1,y1) (x2,y2) ->
	sqrt(((x2-.x1)*.(x2-.x1))+.((y2-.y1)*.(y2-.y1)))

let upper_right (x,y) = x > 0.0 && y > 0.0

let triangle_perimeter : float * float -> float * float -> float * float -> float =
	fun (x1,y1) (x2,y2) (x3,y3) ->
	(distance (x1,y1) (x2,y2)) +. (distance (x2,y2) (x3,y3)) +. (distance (x3,y3) (x1,y1))
