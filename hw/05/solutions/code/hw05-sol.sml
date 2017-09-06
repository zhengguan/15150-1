use "../../code/lib.sml";

(* Task 3.1 *)
(* toInt : int -> int list -> int
   REQUIRES: base > 1 and digits are 
             list of digits in the given base
   ENSURES: converts the digits in base b to an SML int
 *)
fun toInt (base : int) (digits : int list) : int = 
    case digits of 
      [] => 0
    | d::digits' => d + base * toInt base digits'

(* HOF alternative implementation, same spec as toInt *)
fun toInt' (base : int) (digits : int list) : int =
    List.foldr (fn(a, b) => a + base * b) 0 digits

(* Task 3.2 *)
(* toBase : int -> int -> int list
   REQUIRES: base > 1, n >= 0
   ENSURES: returns the representation of n in base b
 *)
fun toBase (base : int) (n : int) : int list =
    case n of
      0 => []
    | _ => n mod base :: toBase base (n div base)

(* Task 3.3 *)
(* convert : int * int -> int list -> int list
   REQUIRES: b1, b2 > 1 and the digits are a list of digits
             in base b1
   ENSURES: converts the digits in b1 to base b2
 *)
fun convert (base1 : int, base2 : int) (L : int list) : int list =
    toBase base2 (toInt base1 L)

val fromBase5 = toInt 5
val 11 = fromBase5 [1, 2]
val 25 = fromBase5 [0, 0, 1]
val 0 = toInt 2 []
val 255 = toInt 16 [15, 15]
val 256 = toInt 16 [0, 0, 1] 

val toBase5 = toBase 5
val [1, 2] = toBase5 11
val [0, 0, 1] = toBase5 25
val [] = toBase 2 0
val [15, 15] = toBase 16 255
val [0, 0, 1] = toBase 16 256

val [1, 2] = convert (10, 5) [1, 1]
val [1] = convert (3, 2) [1]
val [0, 1] = convert (2, 3) [1, 1]

(* Task 4 *)
(* dotProduct : real list * real list -> real
   REQUIRES: a and b are the same length
   ENSURES: returns sum of the pairwise product of the vectors
 *)
fun dotProduct (a : real list, b : real list) : real =
    List.foldr (op +) 0.0 (map (op * ) (zip (a, b)))

(* Tests *)
val true = Real.== (dotProduct (nil, nil), 0.0)
val true = Real.== (dotProduct ([1.0], [1.0]), 1.0)
val true = Real.== (dotProduct ([1.0], [5.0]), 5.0)
val true = Real.== (dotProduct ([6.0], [1.0]), 6.0)
val true = Real.== (dotProduct ([1.0,2.0,3.0], [1.0,2.0,3.0]), 14.0)

(* magnitudeOfVector : real list -> real
   REQUIRES: true
   ENSURES: returns the magnitude of the input vector
 *)
fun magnitudeOfVector (a : real list) : real =
    Math.sqrt(List.foldr (op +) 0.0 (List.map (fn x => x * x) a))

(* Tests *)

val true = Real.== (magnitudeOfVector (nil), 0.0)
val true = Real.== (magnitudeOfVector ([1.0]), 1.0)
val true = Real.== (magnitudeOfVector ([3.0,4.0]), 5.0)
val true = Real.== (magnitudeOfVector ([9.0,40.0]), 41.0)

(* angleBetweenVectors : real list * real list -> real
   REQUIRES: a and b are the same length
   ENSURES: returns the angle (rad) between the input vectors
 *)
fun angleBetweenVectors (a : real list, b : real list) : real =
    let
      val dotProd = dotProduct(a, b)
      val magMultip = magnitudeOfVector(a) * magnitudeOfVector(b)
    in
      Math.acos (dotProd / magMultip)
    end

(* Tests *)
(* almostEqual : real * real -> bool
   REQUIRES: true
   ENSURES: returns true if x and y are close
 *)
fun almostEqual (x, y) = (Real.abs(x-y) < 0.0001)

val true = almostEqual(0.0, 0.0);
val false = almostEqual(0.0, 1.0);
val true = almostEqual(0.0, 0.00009);
val false = almostEqual(0.0, 0.0001);

(* Div by zero : val true = Real.== (angleBetweenVectors (nil, nil), 0.0) *)
val true = almostEqual(angleBetweenVectors ([1.0], [1.0]), 0.0)
val true = almostEqual(angleBetweenVectors ([1.0,2.0,3.0], [3.0,4.0,5.0]), 0.186238765865)
val true = almostEqual(angleBetweenVectors ([1.0,2.0,3.0], [3.0,6.0,5.0]), 0.289751701436)

(* extract : (('a -> bool) * 'a list) -> ('a * 'a list) option
 * REQUIRES: true
 * ENSURES:
 * extract (p, l) evaluates to NONE if none of the elements of l
 * satisfy the predicate p. Otherwise, if x is the first element
 * satisfying p and l = l' @ (x :: l''), then evaluates to SOME (x, l' @ l'').
 *)
fun extract (p : 'a -> bool, l : 'a list) : ('a * 'a list) option =
    case l of
      [] => NONE
    | x :: xs =>
      if p x then
        SOME (x, xs)
      else 
        case extract (p, xs) of
          NONE => NONE
        | SOME (witness, rest) => SOME (witness, x :: rest)
      
fun oddP (x:int) : bool = (x mod 2) = 1

val SOME (3,[2,4]) = extract(oddP, [2,3,4])
val SOME (3,[2,4,5,6,7,8]) = extract(oddP, [2,3,4,5,6,7,8])
val NONE = extract(fn x => not (oddP x), [1,3,5,7,9])
val NONE = extract(oddP, [2,4,6])
val SOME ("b", ["aaa", "bca"]) =
    extract(fn x => String.size x < 2, ["aaa","b","bca"])

(* ---------------------------------------------------------------------- *)
(* Section 5 - Block World *)
(* Task 5.1 *)
(* extractMany (('a * 'a -> bool) * 'a list * 'a list) -> 'a list option
 * REQUIRES: eq is a total equality function on 'a.
 * ENSURES:
 * extractMany eq toExtract from:
 *  - returns NONE if toExtract is not a sub-multi-set of from according to
 *    the equality predicate eq
 *  - otherwise, returns SOME l where l contains the result of removing the
 *    sub-multi-set toExtract from the multiset from. *)
fun extractMany (eq : 'a * 'a -> bool,
                 toExtract : 'a list, from : 'a list) : ('a list) option =
    case toExtract of
      [] => SOME from
    | e :: es =>
      (case extract (fn x => eq (x, e), from) of
         NONE => NONE
       | SOME (_ , from') => extractMany (eq, es, from'))

val SOME [2,4,6] = extractMany(op=,[1,3,5],[1,2,3,4,5,6])
val NONE = extractMany(op=,[1,3,5],[2,4,6,8])
val SOME ["gold",
          "silver",
          "bronze",
          "copper",
          "brass"] = extractMany(op=, ["blue","white","red","green","black"],
                                     ["blue","gold","white","silver","bronze",
                                      "red","green","copper","black","brass"])

(* Task 5.2 *)
datatype block = A | B | C

datatype move =
    PickUpFromBlock of block * block
  | PutOnBlock of block * block
  | PickUpFromTable of block
  | PutOnTable of block

datatype fact =
    Free of block
  | On of block * block
  | OnTable of block
  | HandIsEmpty
  | HandHolds of block

type state = fact list

(* Task 5.3 *)
val initial : state = [HandIsEmpty,
                       OnTable A, OnTable B, OnTable C,
                       Free A, Free B, Free C]

(* instantiates extractMany with equality for your fact datatype *)
fun extractManyFacts (toConsume : fact list, s : state) : state option =
    extractMany (fn (x : fact, y : fact) => x = y, toConsume, s)

(* Task 5.4 *)
(* consumeAndAdd : (state * fact list * fact list) -> state option
 * ENSURES:
 * consumeAndAdd s bef aft:
 *  - returns NONE if bef is not a sub-multi-set of s
 *  - otherwise, returns SOME s', where s' = (s - bef) + aft.
 *    (- sub-multi-set difference, + sub-multi-set union).
 *)
fun consumeAndAdd (s : state, bef : fact list, aft : fact list) : state option =
    case extractManyFacts (bef, s) of
        NONE => NONE
      | SOME s' => SOME (aft @ s')

val NONE = consumeAndAdd(initial, [On(B,C), Free A],[HandHolds A])
val SOME [On (A,B),
          HandIsEmpty,
          OnTable B,
          OnTable C,
          Free C] = consumeAndAdd(initial,[OnTable A, Free A, Free B],
                                          [On (A,B)])
val SOME [HandHolds A,
          OnTable B] = consumeAndAdd([OnTable B, OnTable A, HandIsEmpty],
                                     [OnTable A, HandIsEmpty],[HandHolds A])

(* Task 5.5 *)
(* step : (move * state) -> state option
 * ENSURES:
 * step m s returns NONE if s does not satisfy the preconditions of m.
 * Otherwise, it returns SOME s', where s' is the result of performing
 * m on s (that is, rendering the preconditions false and establishing
 * the postconditions).
 *)
fun step (m : move, s : state) : state option =
    case m of
      PickUpFromBlock (a, b) =>
      consumeAndAdd (s,
                     [Free a , On(a, b) , HandIsEmpty],
                     [Free b , HandHolds a])
    | PutOnBlock (a, b) =>
      consumeAndAdd (s,
                     [HandHolds a, Free b],
                     [HandIsEmpty, Free a, On(a, b)])
    | PickUpFromTable a =>
      consumeAndAdd (s,
                     [Free a, OnTable a, HandIsEmpty],
                     [HandHolds a])
    | PutOnTable a =>
      consumeAndAdd (s,
                     [HandHolds a],
                     [HandIsEmpty, OnTable a, Free a])

val NONE = step (PickUpFromBlock(A,B),initial)
val SOME [HandHolds C,OnTable A,
          OnTable B,Free A,Free B] = step (PickUpFromTable C, initial)
val SOME [HandIsEmpty,
          Free C,
          On (C,A),
          OnTable A,
          OnTable B,
          Free B] = step (PutOnBlock(C,A), [HandHolds C,OnTable A,
                                            OnTable B,Free A,Free B])

(* ---------------------------------------------------------------------- *)
(* Section 7 *)

(* Task 7.1 *)
(* shrubMap : ('a -> 'b) -> 'a shrub -> 'b shrub
   REQUIRES: f is total
   ENSURES: returns the shrub with f applied to all leaves
 *)
fun shrubMap (f : 'a -> 'b) (s : 'a shrub) : 'b shrub =
    case s of 
      Leaf el => Leaf(f el)
    | Branch(l, r) => Branch(shrubMap f l, shrubMap f r)

(* shrubCombine : ('a * 'a -> 'a) -> 'a -> 'a shrub -> 'a
   REQUIRES: f is total and associative, i is an identity on f
   ENSURES: returns the reduction of the shrub with f
 *)
fun shrubCombine (f : 'a * 'a -> 'a) (i : 'a) (s : 'a shrub) : 'a =
    case s of
      Leaf el => f(i, el)
    | Branch(l, r) => f(shrubCombine f i l, shrubCombine f i r) 

(* TEST CASES *)
val s = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
val s'= Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

val Branch(Leaf(2), Branch(Leaf(3), Leaf(4))) =
    shrubMap (fn(i) => i + 1) s
val Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))) =
    shrubMap (Int.toString) s
val Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8))) =
    shrubMap (fn(i) => i * 2) s'

val 6 = shrubCombine (op +) 0 s
val 10 = shrubCombine (op +) 0 s'
val 24 = shrubCombine (op * ) 1 s'
