(* ---------------------------------------------------------------------- *)
(* For Sec 7.1 *)
fun intToString (x : int) : string = Int.toString x
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)

(* Code from class *)

(* sum : int list -> int*)
(* REQUIRES: true*)
(* ENSURES: sum(L) evaluates to the sum of the integers in L.*)
fun sum ([ ] : int list) : int = 0
 |  sum (x::L) = x + (sum L);

(* count : int list list -> int*)
(* REQUIRES: true*)
(* ENSURES: count(R) evaluates to the sum of the integers in R.*)
fun count ([ ] : int list list) : int = 0
 |  count (r::R) = (sum r) + (count R)

(*Task 7.2*)
(*mult: int list -> int*)
(*REQUIRES: true*)
(*ENSURES: mult(L) evaluates to the product of integers in L*)
fun mult ([] : int list) = 1
  | mult (x :: L) = x * mult(L)

val 1 = mult []
val 5 = mult [5]
val 42 = mult [2, 21]

(*Task 7.3*)
(*mult' : int list * int -> int*)
(*REQUIRES: true*)
(*ENSURES: mult'(L, a) evaluates to the product of integers in L times a*)
fun mult' ([] : int list, a : int) : int = a
  | mult' (x::L, a) = mult'(L, x * a)

val 10 = mult' ([], 10)
val 3 = mult' ([3], 1)
val 60 = mult' ([2, 1, 15], 2)

(*Task 7.4*)
(*Mult : int list list -> int*)
(*REQUIRES: true*)
(*ENSURES: Mult(R) evaluates to the product of integers in R*)

fun Mult ([] : int list list) : int = 1
  | Mult (r :: R) = (mult r) * (Mult R)

val 1 = Mult []
val 1 = Mult [[]]
val 10 = Mult [[10]]
val 35 = Mult [[7, 5]]
val 21 = Mult [[1, 3], [7]]

(*Task 7.5*)
(*Mult : int list list * int -> int*)
(*REQUIRES: True*)
(*ENSURES: Mult (R, a) evaluates to the product of integers in R times a*)

fun Mult' ([] : int list list, a : int) = a
  | Mult' (r::R, a) = Mult' (R, mult' (r, 1) * a)

val 5 = Mult' ([], 5)
val ~16 = Mult' ([[~4]], 4)
val 180 = Mult' ([[2, 2, ~1], [15, ~1], [3]], 1)
