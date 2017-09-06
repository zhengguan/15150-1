(* pi: real *)
val pi : real = 3.14159;

(* fact: int -> int *)
fun fact (0 : int) : int = 1
  | fact n = n * (fact (n - 1))

(* f : int -> int *)
fun f (9 : int) = 4
  | f _ = 5

(* circ : real -> real *)
fun circ (r : real) : real = 2.0 * pi * r;

(* semicirc : real -> real *)
fun semicirc (r : real) : real = pi * r;

(* area : real -> real *)
fun area (d : real) : real = pi * d * d;

