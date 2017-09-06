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
 |  sum (x::L) = x + (sum L)

(* count : int list list -> int*)
(* REQUIRES: true*)
(* ENSURES: count(R) evaluates to the sum of the integers in R.*)
fun count ([ ] : int list list) : int = 0
 |  count (r::R) = (sum r) + (count R)

(* mult : int list -> int*)
(* REQUIRES: true*)
(* ENSURES: mult(L) evaluates to the product of the integers in L.*)
fun mult [ ] = 1
| mult (x::L) = x * (mult L);

(* mult’ : int list * int -> int *)
(* REQUIRES: true*)
(* ENSURES: mult’(L, a) evaluates to the product of a and the integers in L.*)
fun mult’ ([ ], a) = a
| mult’ (x::L, a) = mult’ (L, x*a);

(* Mult : int list list -> int *)
(* REQUIRES: true*)
(* ENSURES: Mult(R) evaluates to the product of all the integers in the lists of R.*)
fun Mult [ ] = 1
| Mult (r::R) = mult(r) * Mult(R);

(* Mult’ : int list list * int -> int*)
(* REQUIRES: true*)
(* ENSURES: Mult'(R) evaluates to the product of a and all the integers in the lists of R.*)
fun Mult' [ ] = 1
| Mult' (r::R) = Mult'(R, mult'(r, a));

