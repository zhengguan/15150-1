(***** Section 3 Higher Order Functions *****)

(* Task 3.1 *)
val double: int -> int = fn x => x * 2
val quadruple : int -> int = fn x => double (double x)

(* Task 3.2 *)
fun thenAddOne ((f:int -> int), (x:int)) : int =
    f x + 1

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

(* mapList : ('a -> 'b) * 'a list -> 'b list *)
(* REQUIRES: f is total *)
(* ENSURES: mapList (f,[x1, ..., xi]) == [f x1, ..., f xi] *)
fun mapList (f: 'a -> 'b, [] : 'a list): 'b list = []
  | mapList (f, x::xs) = (f x)::(mapList (f, xs))

(* Task 3.4 *)

(* mapList' : ('a -> 'b) -> ('a list -> 'b list) *)
(* REQUIRES: f is total *)
(* ENSURES: mapList f [x1, ..., xi] == [f x1, ..., f xi] *)
fun mapList' (f : 'a -> 'b) (L : 'a list) : 'b list =
    case L of
      [] => []
    | x::xs => (f x)::(mapList' f xs)

(* Task 3.5 *)
val [] = mapList(double, [])
val [] = mapList(quadruple, [])
val [2,4,6] = mapList(double, [1,2,3])
val [4,8,12] = mapList(quadruple, [1,2,3])
val [15,150] = mapList(fn x => x + 10, [5, 140])

val [] = mapList' double []
val [4,8,12] = mapList' quadruple [1,2,3]
val [~2,4,~5] = mapList' (fn x => ~x) [2,~4,5]


(***** Section 4 Options ******)
(* Task 4.1 *)

(* findOdd: int list -> int option *)
(* REQUIRES : true *)
(* ENSURES : findOdd(L) returns SOME(x) iff
 * there is at least one odd number x in L *)
fun findOdd([] : int list): int option = NONE
  | findOdd(x::xs) = case x mod 2 of
                        1 => SOME x
                      | _ => findOdd xs

val NONE = findOdd []
val NONE = findOdd [2,4,6]
(* Note for these we're just testing that we get back SOME.
 * so saying val SOME x would be sufficient as well *)
val SOME 1 = findOdd [1,3,5]
val SOME 3 = findOdd [2,3,4,5,6,7,8]

(* subsetSumCert provided for your convenience *)
fun subsetSumCert (nil : int list, s : int): bool * int list = (s=0, nil)
  | subsetSumCert (x::xs, s) =
    case subsetSumCert (xs, s-x) of
      (true, l1) => (true, x::l1)
    | (false, _) => subsetSumCert (xs,s)

(* Task 4.2 *)

(* subsetSumOption : int list * int -> int list option *)
(* REQUIRES: true *)
(* ENSURES: subsetSumOption(l,s) == SOME l' iff l' is a sublist
 * of l that sums to s and NONE if no such l' exists *)
fun subsetSumOption (l : int list, 0 : int) : int list option = SOME []
  | subsetSumOption ([], _) = NONE
  | subsetSumOption (x::xs, s) =
    case subsetSumOption(xs, s-x) of
      SOME(l) => SOME(x::l)
    | NONE => subsetSumOption(xs, s)

val NONE = subsetSumOption([],1)
val NONE = subsetSumOption([1,2,3],~1)
val SOME [] = subsetSumOption([], 0)
val SOME [] = subsetSumOption([1,2,3,4],0)
