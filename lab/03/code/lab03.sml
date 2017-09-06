(* you can remove this defintion when you're done to make sure you didn't
 * miss any functions
 *)
exception Unimplemented

(* Task 2.1 *)
(* append : int list * int list -> int list *)
(* REQUIRES: L1 and L2 are type int list *)
(* ENSURES: Appends L1 to L2 *)
fun append ([] : int list, L2 : int list) : int list = L2
  | append (x::L : int list, L2 : int list) : int list = append(L, x::L2)

(* Task 3.1 *)
(* reverse : int list -> int list *)
(* REQUIRES: L is type int list *)
(* ENSURES: reverses L *)
fun reverse ([] : int list) : int list = []
  | reverse (x::L : int list) : int list = reverse(L) @ [x]

(* moveFront' : int list -> int list *)
(* REQUIRES: L1 and L2 are type int list *)
(* ENSURES: move first element of L2 to front of L1 *)
fun moveFront (L1 : int list, [] : int list) : int list = L1
  | moveFront(L1 : int list, x::L : int list) : int list = moveFront(x::L1, L)

(* Task 3.3 *)
(* moveFront' : int list -> int list *)
(* REQUIRES: L1 and L2 are type int list *)
(* ENSURES: move first element of L2 to front of L1 *)
fun moveFront (L1 : int list, [] : int list) : int list = L1
  | moveFront(L1 : int list, x::L : int list) : int list = moveFront(x::L1, L)

(* reverse' : int list -> int list *)
(* REQUIRES: L is type int list *)
(* ENSURES: reverses L *)
fun reverse' ([] : int list) : int list = []
  | reverse' (x::L : int list) : int list = moveFront([x],L)


fun fib (0 : int) : int = 1
  | fib 1               = 1
  | fib n               = fib (n-1) + fib (n-2)

(* Task 5.1*)
(* sumPair : int * int -> int *)
(* REQUIRES: n and m are type int *)
(* ENSURES: sumPair(x,y) evalutes to x+y *)
fun sumPair (n : int, m : int) : int = n + m

(* fibber : int -> int * int *)
(* REQUIRES: n>=0 *)
(* ENSURES: fibber(n) returns (fib(n), fib(n+1)) *)
fun fibber (0 : int) : int * int = (0, 1)
  | fibber (n : int) : int * int = (sumPair(fibber(n-2)), sumPair(fibber(n-1)))

(* Task 6.1 *)
(* merge : int list * int list -> int list *)
(* REQUIRES: L1 and L2 are sorted *)
(* ENSURES: merge(L1, L2) merges L1 and L2 and is sorted *)
fun merge (L1 : int list, [] : int list) : int list = L1
  | merge ([] : int list, L2 : int list) : int list = L2
  | merge (x::L1 : int list, y::L2 : int list) : int list =
    if x < y
      then x::merge(L1, y::L2)
      else y::merge(x::L1, L2)

(* Task 7.1 *)
(* evens : int list -> int list *)
(* REQUIRES: L is type int list *)
(* ENSURES: evens(L) evalutes to a list of the even numbers in L *)
fun evens ([] : int list) : int list = []
  | evens (x::L : int list) : int list = 
    if x mod 2 = 0
      then x::evens(L)
      else evens(L)

(* Task 7.2 *)
(* DOCUMENT THIS FUNCTION *)
fun bitAnd (L1 : int list, L2 : int list) : int list =
  raise Unimplemented

(* Task 7.3 *)
(* DOCUMENT THIS FUNCTION *)
fun interleave (L1 : int list, L2 : int list) : int list =
  raise Unimplemented

