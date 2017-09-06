(***** Section 3 Higher Order Functions *****)

(* Task 3.1 *)
val double: int -> int = fn x => 2 * x
val quadruple : int -> int = fn x => double(double(x))

(* Task 3.2 *)
fun thenAddOne ((f:int -> int), (x:int)) : int = f(x) + 1

(* Tests for thenAddOne *)
val 5 = thenAddOne(double, 2)
val 13 = thenAddOne(quadruple, 3)
val ~1 = thenAddOne(fn x => ~x, 2)

(* doubleList and quadrupleList reproduced for your convenience *)

fun doubleList ([] : int list): int list = []
  | doubleList (x::xs) =
    (double x)::(doubleList xs)

fun quadrupleList ([]:int list): int list = []
  | quadrupleList (x::xs) =
    (quadruple x)::(quadrupleList xs)

(* Task 3.3 *)
fun mapList (f: 'a -> 'b, [] : 'a list): 'b list = []
  | mapList (f: 'a -> 'b, x::L : 'a list): 'b list = (f x)::mapList(f, L)

(* Task 3.4 *)
fun mapList' (f: 'a -> 'b) (L : 'a list) : 'b list = mapList(f, L)

(* Task 3.5 *)
(* Tests for mapList *)
val [2,4,6] = mapList(double, [1,2,3])
val [4,12] = mapList(quadruple, [1,3])
val [~3,~4,~5] = mapList(fn x => ~x, [3,4,5])

(* Tests for mapList' *)
val [2,4,6] = mapList'(double) ([1,2,3])
val [4,12] = mapList'(quadruple) ([1,3])
val [~3,~4,~5] = mapList'(fn x => ~x) ([3,4,5])


(***** Section 4 Options ******)
(* Task 4.1 *)
fun findOdd ([] : int list) : int option = NONE
  | findOdd (x::l : int list) : int option = 
    if x mod 2 = 1
      then SOME x
      else findOdd l

(* Tests for fimdOdd *)
val SOME 3 = findOdd([2,3,4,5])
val NONE = findOdd([2,4])
val NONE = findOdd([])

(* subsetSumCert provided for your convenience *)
fun subsetSumCert (nil : int list, s : int): bool * int list = (s=0, nil)
  | subsetSumCert (x::xs, s) =
    case subsetSumCert (xs, s-x) of
      (true, l1) => (true, x::l1)
    | (false, _) => subsetSumCert (xs,s)

(* Task 4.2 *)
fun subsetSumOption (l : int list, 0 : int) : int list option = SOME []
  | subsetSumOption (nil : int list, s : int) : int list option = NONE
  | subsetSumOption (x::xs, s) =
    case subsetSumOption (xs, s-x) of
      SOME l1 => SOME (x::l1)
    | NONE => subsetSumOption (xs,s)

(* Tests for subsetSumOption *)
val SOME [1,2,4] = subsetSumOption([1,2,9,4], 7)
val NONE  = subsetSumOption([1,2,3], 8)
