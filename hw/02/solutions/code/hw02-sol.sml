(* evenP : int -> bool
 * REQUIRES: true
 * ENSURES: evenP n returns true if n is even and false otherwise
 *)
fun evenP (n : int) : bool = (n mod 2 = 0)

(* oddP : int -> bool
 * REQUIRES: true
 * ENSURES: oddP n returns true if n is odd and false otherwise
 *)
fun oddP (n : int) : bool = (n mod 2 = 1)

(* add : int * int -> int
 * REQUIRES:  n, m >= 0
 * ENSURES:  add(n,m) = n+m
 *)
fun add (0 : int, m : int) : int = m
  | add (n : int, m : int) : int = 1 + add(n-1, m)

val 7 = add(0,7)
val 17 = add(10,7)


(* leq : int * int -> bool 
 * REQUIRES: x, y >= 0 
 * ENSURES: leq (x,y) returns true if x <= y and false otherwise 
 *)
fun leq (0 : int, y : int) : bool = true
  | leq (x : int, 0 : int) : bool = false
  | leq (x : int, y : int) : bool = leq(x-1, y-1)

val true = leq(0,7)
val false = leq(9,7)


(* halfSum : int -> real
 * REQUIRES: n >= 0
 * ENSURES:  halfSum(n) returns the nth number in the halfSum series.
 *)
fun halfSum (0 : int) : real = 0.0
  | halfSum (n : int) : real = 0.5 + 0.5 * halfSum(n-1)

val true = Real.==(halfSum 0, 0.0)
val true = Real.==(halfSum 1, 0.5)
val true = Real.==(halfSum 2, 0.75)
val true = Real.==(halfSum 4, (1.0/2.0) + (1.0/4.0) + (1.0/8.0) + (1.0/16.0))


(* altHalfSum : int -> real
 * REQUIRES: n >= 0
 * ENSURES:  altHalfSum(n) returns the nth number in the altHalfSum series.
 *)
fun altHalfSum (0 : int) : real = 0.0
  | altHalfSum (n : int) : real = 0.5 - 0.5 *  altHalfSum(n-1)

val true = Real.==(altHalfSum 0, 0.0)
val true = Real.==(altHalfSum 1, 0.5)
val true = Real.==(altHalfSum 2, 0.25)
val true = Real.==(altHalfSum 4, (1.0/2.0) - (1.0/4.0) + (1.0/8.0) - (1.0/16.0))

(* is_prime_helper : int * int -> bool
 * REQUIRES: n > 1, k > 0
 * ENSURES: is_prime_helper(n, k) returns true if n is prime and false otherwise
 *)
fun is_prime_helper (n : int, k : int) : bool = 
    case (k, n mod k) of
      (1, _) => true
    | (_, 0) => false
    |   _    => is_prime_helper (n, k - 1)
val true = is_prime_helper(2, 1)
val true = is_prime_helper(5, 4)
val false = is_prime_helper(78, 77)

(* is_prime : int -> bool
 * REQUIRES: n > 1
 * ENSURES: is_prime(n) returns true if n is prime and false otherwise
 *)
fun is_prime (n : int) : bool = is_prime_helper(n, n-1)
val true = is_prime 2
val true = is_prime 19
val false = is_prime 65
