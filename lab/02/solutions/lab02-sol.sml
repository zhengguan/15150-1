(* summ : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES:  summ n returns the sum of the integers 0 through n. *)
fun summ (0 : int) : int = 0
  | summ n = n + summ(n - 1)

(* Tests *)
val 0 = summ(0)
val 10 = summ(4)
val 15 = summ(5)

(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)
fun double (0 : int) : int = 0
  | double n = 2 + double (n - 1)

(* Tests for double *)
val 0 = double 0
val 4 = double 2

(* square : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: square n returns n * n *)
fun square (0 : int) = 0
  | square n = square (n - 1) + double (n) - 1

(* Tests for square *)
val 0 = square 0
val 1 = square 1
val 4 = square 2
val 9 = square 3
val 289 = square 17
val 324 = square 18

(* evenP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: evenP n evaluates to true iff n is even. *)
fun evenP (0 : int) : bool = true
  | evenP 1 = false
  | evenP n = evenP (n - 2)

(* Tests for evenP *)
val true = evenP 0
val false = evenP 1
val true = evenP 12
val false = evenP 27

(* oddP : int -> bool*)
(* REQUIRES: n >= 0 *)
(* ENSURES: oddP n returns true if and only if n is odd.*)
fun oddP (0 : int) : bool = false
  | oddP 1 = true
  | oddP n = oddP (n - 2)

(* Tests for oddP *)
val false = oddP 0
val true = oddP 1
val false = oddP 6
val true = oddP 37

(* divisibleByThree : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: divisibleByThree n returns true iff n is divisble by three. *)
fun divisibleByThree (0 : int) : bool = true
  | divisibleByThree 1 = false
  | divisibleByThree 2 = false
  | divisibleByThree n = divisibleByThree (n - 3)

(* Tests for divisibleByThree *)
val true = divisibleByThree 0
val false = divisibleByThree 1
val false = divisibleByThree 2
val true = divisibleByThree 6
val false = divisibleByThree 7
val false = divisibleByThree 11

(* GCD: int * int -> int *)
(* REQUIRES:  m, n >= 0    *)
(* ENSURES:   GCD (m, n) returns the g.c.d. of m and n *)
fun GCD (m: int, 0): int = m
  | GCD (0, n: int): int = n
  | GCD (m: int, n: int): int =
        if m > n then GCD(m mod n, n) else GCD(m, n mod m)

(* Tests for GCD *)
val 0 = GCD(0, 0)
val 23 = GCD(0, 23)
val 7 = GCD(14, 21)

(* stein : int * int -> int *)
(* REQUIRES:  m, n > 0        *)
(* ENSURES:   stein(m,n) returns the g.c.d. of m and n. *)
fun stein(m,n) =
    if m=n then m else
       if m mod 2 = 0
       then
           if n mod 2 = 0
           then 2 * stein(m div 2, n div 2)
           else stein(m div 2, n)
       else if n mod 2 = 0
            then stein(m, n div 2)
            else if n>m
                 then stein(m, (n-m) div 2)
                 else stein((m-n) div 2, n)

(* Tests for stein *)
val 7 = stein(14, 21)
val 9 = stein(36,45)
val 1 = stein(7,9)

(* stein' : int * int -> int *)
(* REQUIRES:  m, n > 0        *)
(* ENSURES:   stein'(m,n) returns the g.c.d. of m and n. *)
fun stein'(m,n) =
    if m=n then m
    else case(m mod 2, n mod 2) of
              (0, 0) => 2 * stein(m div 2, n div 2)
            | (0, 1) => stein(m div 2, n)
            | (1, 0) => stein(m, n div 2)
            (* The case below is for (1,1). We use the wildcard
             * character below to prevent non-exhaustive warnings
             *)
            | (_, _) => if n>m then stein(m, (n-m) div 2)
                        else stein((m-n) div 2, n)

(* Tests for stein' *)
val 7 = stein'(14, 21)
val 9 = stein'(36,45)
val 1 = stein'(7,9)
