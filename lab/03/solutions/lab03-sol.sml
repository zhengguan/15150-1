(* append: int list * int list -> int list*)
(* REQUIRES: true *)
(* ENSURES append(L1,L2) evaluates to a list with all the elements of L1
 * in their original order followed by all the elements of L2 in their
 * original order
 *)
fun append (L1 : int list, L2 : int list) : int list =
  case L1 of
      [] => L2
    | x :: L => x :: (append (L, L2))

(* tests for append *)
val [] = append([],[])
val [1,2,3] = append([],[1,2,3])
val [4,2,4,0,0,0] = append([4,2,4], [0,0,0])

(* reverse: int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: reverse(L) evalautes to a list that contains all
 *  the elements of L in reverse order.
 *)
fun reverse (L : int list) : int list =
  case L of
       [] => []
     | x::R => (reverse R) @ [x]

(* tests for reverse *)
val [] = reverse []
val [2,1] = reverse [1,2]
val [1,2,2,3,4,5,5] = reverse [5,5,4,3,2,2,1]

(* reverse': int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: reverse'(L) evalautes to a list that contains all
 *  the elements of L in reverse order.
 *)
fun reverse' (L : int list) : int list =
    let
      fun reverse'' (L : int list, A : int list) : int list =
        case L of
             [ ] => A
           | x::R => reverse'' (R, x :: A)
    in
      reverse'' (L, [ ])
    end

(* tests for reverse' *)
val [] = reverse' []
val [2,1] = reverse' [1,2]
val [1,2,2,3,4,5,5] = reverse' [5,5,4,3,2,2,1]

(* fibber : int -> (int * int) 			     *)
(* REQUIRES n >= 0							 *)
(* ENSURES fibber(n) evaluates to (fib(n), fib(n+1)) *)
fun fibber (0: int) : int * int = (1, 1)
  | fibber (n: int) : int * int =
      let
        val (x: int, y: int) = fibber (n-1)
      in (y, x + y)
      end

(* tests for fibber *)
val (1,1) = fibber 0
val (1,2) = fibber 1
val (144, 233) = fibber 11

(* Task 6.1*)
(* merge: int list * int list => int list *)
(* REQUIRES: L1 and L2 are sorted in increasing order *)
(* ENSURES: merge(L1, L2) evaluates to a list L sorted in increasing order that
 * contains the elements of L1 and L2
 *)
fun merge (L1 : int list, L2 : int list) : int list =
    case (L1, L2)
     of ([], L2) => L2
      | (L1, []) => L1
      | (x :: xs, y :: ys) =>
        case (Int.compare(x,y))
         of LESS => x :: (merge(xs, y :: ys))
          | _ =>    y :: (merge(x :: xs, ys))

(* tests for merge *)
val [] = merge([],[])
val [1,2] = merge([1,2],[])
val [0,1,2,3,4,5,7,9] = merge([1,3,5,7,9],[0,2,4])

(* evens : int list -> int list				*)
(* REQUIRES length(L) >= 0					*)
(* ENSURES evens(L) evaluates to a list consisting
	of all the integers in L that are even  *)
fun evens (l: int list) : int list =
  case l of
    [] => []
  | x::xs => (case x mod 2 of
                0 => x :: evens(xs)
              | _ => evens(xs))

val [2,4] = evens [1,2,3,4,5]
val [] = evens []
val [0,2,4,6] = evens [0,2,4,6]
val [] = evens [1,3,5]

(* bitAnd: int list * int list -> int list
 * REQUIRES: true
 * ENSURES: bitAnd(A,B) evaluates to  a list with
 * a logical and performed on the
 * corresponding elements of the two lists
 *
 * examples:
 *   bitAnd ([1, 1, 0, 1], [0, 0, 1, 0]) == [0, 0, 0, 0]
 *   bitAnd ([1, 1, 0], [1, 1, 0, 1]) == [1, 1, 0, 0]
 *   bitAnd ([], [1, 1, 1]) == [0, 0, 0]
 *)

fun bitAnd ([] : int list, [] : int list) : int list = []
  | bitAnd ([] : int list, x::xs : int list) : int list = 0 :: bitAnd ([], xs)
  | bitAnd (x::xs : int list, [] : int list) : int list = 0 :: bitAnd (xs, [])
  | bitAnd (x::xs : int list, y::ys : int list) : int list =
        case (x, y) of
            (1, 1) => 1 :: bitAnd (xs, ys)
          | _ => 0 :: bitAnd (xs, ys)

val [0, 0, 0, 0] = bitAnd ([1, 1, 0, 1], [0, 0, 1, 0])
val [1, 1, 0, 0] = bitAnd ([1, 1, 0], [1, 1, 0, 1])
val [0, 0, 0] = bitAnd ([], [1, 1, 1])

(* interleave : int list * int list -> int list *)
(* REQUIRES length(A), length(B) >= 0           *)
(* ENSURES interleave(A,B) evaluates to an interleaved list with
           elements from A and B                *)
fun interleave(A : int list,B : int list) =
  case (A,B) of
   ([],_) => B
   |(_,[]) => A
   |(x::xs,y::ys) => x::y::interleave(xs,ys)

val [1,3,2,4] = interleave([1,2],[3,4])
val [] = interleave([],[])
val [1,2] = interleave([],[1,2])
val [1,2] = interleave([1,2],[])



