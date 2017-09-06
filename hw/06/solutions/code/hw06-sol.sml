(* toInt2 : int list -> int
 * REQUIRES: All elements of L are either 0 or 1.
 * ENSURES: toInt2 L evaluates to the integer whose binary representation
 * with most significant digit in the leftmost place is L.
 *)
fun toInt2 (L : int list) : int =
    if List.all (fn x => x = 0 orelse x = 1) L
    then
      List.foldl (fn (a,b) => a + (2 * b)) 0 L
    else
      raise Fail "An element of the list was too large"

(* Tests for toInt2 *)
val 0 = toInt2 []
val 0 = toInt2 [0]
val 1 = toInt2 [1]
val 2 = toInt2 [1,0]
val 3 = toInt2 [1,1]
val 3 = toInt2 [0,1,1]
val 14 = toInt2 [1,1,1,0]

(* allZeros : (int * int list) list -> bool
 * REQUIRES: true
 * ENSURES: allZeros L will be true iff for every element (x,ds) of L, x is 0,
 * and false otherwise.
 *)
fun allZeros (L : (int * int list) list) : bool = List.all (fn (x,_) => x = 0) L

(* Tests for allZeros*)
val true = allZeros []
val true = allZeros [(0,[])]
val true = allZeros [(0,[1,2,3]),(0,[0,0,0]),(0,[])]
val false = allZeros [(1,[])]
val false = allZeros [(1,[0,0,0])]
val false = allZeros [(0,[1,2,3]),(1,[0,0,0,0]),(0,[0,0])]

(* exp2 : int -> int
 * REQUIRES: x >= 0
 * ENSURES: exp2 x evaluates to 2^x.
 *)
fun exp2 0 = 1
  | exp2 x = 2 * (exp2 (x - 1))

(* Tests for exp2 *)
val 1 = exp2 0
val 2 = exp2 1
val 4 = exp2 2
val 8 = exp2 3
val 16 = exp2 4

(* rep : (int * int list) -> int
 * REQUIRES: x > 0, and for all d in ds, d is either zero or one.
 * ENSURES: rep (x,ds) = x * 2^(length ds) + toInt2 ds
 *)
fun rep (x : int, ds : int list) : int = x * exp2(length ds) + (toInt2 ds)

(* Tests for rep *)
val 0 = rep (0,[])
val 1 = rep (1,[])
val 1 = rep (0,[1])
val 11 = rep (11,[])
val 11 = rep (5,[1])
val 11 = rep (2,[1,1])
val 11 = rep (1,[0,1,1])
val 11 = rep (0,[1,0,1,1])

(* divmod : (int * int list) -> (int * int list)
 * REQUIRES: x >= 0
 * ENSURES: divmod x evaluates to (y,z) such that 2 * y + z = x.
 *)
fun divmod (x : int, ds : int list) : (int * int list) =
    (x div 2, (x mod 2)::ds)

(* Tests for divmod *)
val (0,[0]) = divmod (0,[])
val (0,[1]) = divmod (1,[])
val (1,[0]) = divmod (2,[])
val (1,[1]) = divmod (3,[])
val (2,[0]) = divmod (4,[])
val (2,[1]) = divmod (5,[])
val (3,[1,1]) = divmod (7,[1])
val (3,[0,1]) = divmod (6,[1])
val (3,[0,0]) = divmod (6,[0])

(* partition : ('a -> bool) -> 'a list -> ('a list * 'a list)
 * REQUIRES: p is total.
 * ENSURES: partition p L evaluates to the pair (L1,L2) such that L1@L2 is
 * a permutation of L, and for all x in L1, p x is true, and for all x in
 * L2, p x is false, and if p x = p y for some x before y in L, then
 * x is before y in L1@L2.
 *)
fun partition (p : 'a -> bool) (L : 'a list) : ('a list * 'a list) =
    (List.filter p L, List.filter (not o p) L)

(* Tests for partition *)
val([],[]) = partition (fn x => false) []
val([],[]) = partition (fn x => true) []
val([],[2,3,5,7,11]) = partition (fn x => false) [2,3,5,7,11]
val([2,3,5,7,11],[]) = partition (fn x => true) [2,3,5,7,11]
val([2],[3,5,7,11]) = partition (fn x => x mod 2 = 0) [2,3,5,7,11]
val([0,2,4,6,8],[1,3,5,7,9]) =
   partition (fn x => x mod 2 = 0) [0,1,2,3,4,5,6,7,8,9]

(* rad : (int * int list) list -> (int * int list) list
 * REQUIRES: If (x,ds) is an element of L, then x >= 0 and for each d in ds,
 * ds = 0 or ds = 1.
 * ENSURES: rad L evaluates to the list L' that is a permutation of L, sorted
 * by the comparison fn (x,y) => Int.compare(rep x, rep y).
 *)
fun rad (L : (int * int list) list) : (int * int list) list =
    if allZeros L
    then
      L
    else
      let
        val (zs,os) = partition
                          (fn (_,[]) =>
                              raise Fail "Impossible. allZeros is true on []"
                            | (x,d::ds) => d = 0)
                          (map divmod L)
      in
        rad(zs@os)
      end

(* radixsort : (int * int list) list -> int list
 * REQUIRES: true
 * ENSURES: radixsort L evaluates to a sorted permutation of L.
 *)
fun radixsort (L : int list) : int list =
    map rep (rad (map (fn x => (x,[])) L))

(* Tests for radixsort *)
val [] = radixsort []
val [1] = radixsort [1]
val [1,2] = radixsort [1,2]
val [1,2] = radixsort [2,1]
val [1,2,3] = radixsort [1,2,3]
val [1,2,3] = radixsort [1,3,2]
val [1,2,3] = radixsort [2,1,3]
val [1,2,3] = radixsort [2,3,1]
val [1,2,3] = radixsort [3,1,2]
val [1,2,3] = radixsort [3,2,1]

(* Combination Continuations *)
(* combos: int list -> int list
 * REQUIRES: elements of L are nonzero
 * ENSURES: combos L returns L' where L' is a list of all combinations of
 *          the integers in L, as defined in problem 6. *)
fun combos ([] : int list) : int list = []
  | combos [x] = [x]
  | combos (x::R) =
        let
          val C = combos R
        in
          (map (fn c => x+c) C)@(map (fn c => x*c) C)
        end

(* Tests for combos *)

val [] = combos []
val [4] = combos [4]
val [4,3] = combos [3,1]
val [6,7,5,6] = combos [1,2,3]

(* make: int -> int list -> string list option
 * REQUIRES: n is nonzero, elems of L are nonzero
 * ENSURES: make n L returns SOME(x), where x is a string list
 *  of "*" and "+" that show how to make n using L,
 *  or NONE if no combination of L exists*)
fun make n [] = NONE
  | make n [x] = if x=n then SOME [] else NONE
  | make n (x::R) = case make (n-x) R of
                    SOME L => SOME ("+"::L)
                  | NONE   => if n mod x = 0
                              then case (make (n div x) R) of
                                        SOME L => SOME("*"::L)
                                      | NONE => NONE
                              else NONE

(* Tests for make *)

val NONE = make 5 []
val NONE = make 5 [12]
val SOME(["*"]) = make 10 [2,5]
val NONE = make 4 [8,46]
val SOME(["+","+","*"]) = make 15 [1,2,3,4]

(* make_C: int -> int list -> (string list -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: n is a nonzero integer, and L contains only nonzero elements
 * ENSURES: make_C n L s k returns s A, where A is a string list of
 *          "*" and "+" that describes a way to combine L to make n if
 *          such a combination exists, or k () if no combination exists *)
fun make_C n [] s k = k ()
  | make_C n [x] s k = if x = n then s [] else k ()
  | make_C n (x::R) s k = make_C (n-x) R
     (*success continuation*)(fn L => s("+"::L))
     (*failure continuation*)(fn () => if n mod x = 0
                              then make_C (n div x) R (fn L => s("*"::L)) k
                              else k())

(* Tests for make_C! *)

val NONE = make_C 5 [] (fn x => SOME x) (fn () => NONE)
val NONE = make_C 5 [12] (fn x => SOME x) (fn () => NONE)
val SOME(["*"]) = make_C 10 [2,5] (fn x => SOME x) (fn () => NONE)
val NONE = make_C 4 [8,46] (fn x => SOME x) (fn () => NONE)
val SOME(["+","+","*"]) = make_C 15 [1,2,3,4] (fn x => SOME x) (fn () => NONE)
