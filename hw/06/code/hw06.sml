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

(* Task 3.1 *)
(* rep : (int * int list) -> int *)
(* REQUIRES: each element in ds is either 0 or 1 *)
(* ENSURES: rep(x,ds) = x * exp2(length(ds)) + toInt2(ds) *)
fun rep (x : int, ds : int list) : int = x * exp2(length(ds)) + toInt2(ds)

(* Tests for rep *)
val 11 = rep(11, [])
val 11 = rep(2, [1,1])
val 13 = rep(3, [0,1])
val 8 = rep(4, [0])
val 5 = rep(0, [1,0,1])
val 0 = rep(0, [])

(* Task 3.2 *)
(* divmod : (int * int list) -> (int * int list) *)
(* REQUIRES: each element in ds is either 0 or 1 *)
(* ENSURES: divmod(x,ds) = (x div 2,(x mod 2)::ds) *)
fun divmod (x : int, ds : int list) : (int * int list) = (x div 2, (x mod 2)::ds)

(* Tests for divmod *)
val (5, [1]) = divmod(11, [])
val (1, [0,1,1]) = divmod(2, [1,1])
val (1, [1,0,1]) = divmod(3, [0,1])
val (2, [0,0]) = divmod(4, [0])
val (0, [0,1,0,1]) = divmod(0, [1,0,1])
val (0, [0]) = divmod(0, [])

(* Task 3.3 *)
(* partition : ('a -> bool) ->'a list -> 'a list * 'a list *)
(* REQUIRES: p is a total function *)
(* ENSURES: partition(p)(L) = (L1,L2) where L1@L2 is a permutation of L, for each x in L1 p(x) = true, for each x in L2 p(x) =  false, and if x is before y in L and p(x) = p(y) then x will be before y in L1@L2 *)
fun partition (p : 'a -> bool) ([] : 'a list) : ('a list * 'a list) = ([], []) 
  | partition (p : 'a -> bool) (x::L : 'a list) : ('a list * 'a list) = 
      let val (a, b) = partition(p) L
      in
        case p(x) of
            true => (x::a, b)
          | false => (a, x::b)
      end

(* Tests for partition *)
val ([], []) = partition (fn x => true) []
val ([0,2,4], [1,3,5]) = partition (fn x => x mod 2 = 0) [0,1,2,3,4,5]
val ([[],[],[]],[[1],[2],[3,4]]) = partition (fn x => x = []) [[],[1],[],[2],[3,4],[]]


(* Task 3.4 *)
(* rad : (int * int list) list -> (int * int list) list *)
(* REQUIRES: for each (x,ds) in L ds must be [] *)
(* ENSURES: rad(L) returns a sorted permutation of the binary representation of each x in L *)
fun rad ([] : (int * int list) list) : (int * int list) list = []
  | rad ((0, [])::L : (int * int list) list) : (int * int list) list = (0, [])::L
  | rad (L : (int * int list) list) : (int * int list) list = 
      if allZeros(L)
        then
          let
            val (a,b) = partition(fn (x,y::R) => y = 0) L
            val f1 = fn (x,y::R) => (x,R);
            val f2 = fn (n,x) => (n,0::x);
            val f3 = fn (n,x) => (n,1::x);
          in
            map(f2) (rad(map(f1) a)) @ map(f3) (rad(map(f1) b))
          end
        else rad(map(divmod) L)

(* Tests for rad *)
val [] = rad([])
val [(0,[1,0,1])] = rad([(5,[])])
val [(0,[1,0,1]),(0,[1,1,0]),(0,[1,1,1])] = rad([(5,[]),(7,[]),(6,[])])
val [(0,[0,1,1]),(0,[1,0,0]),(0,[1,0,1])] = rad([(3,[]),(4,[]),(5,[])])

(* Task 3.5 *)
(* Task 3.4 *)
(* radixsort : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: rad(L) returns a sorted permutation of L *)
fun radixsort ([] : int list) : int list = []
  | radixsort (L : int list) : int list = map(fn (x,y) => toInt2(y)) (rad(map(fn x => (x,[])) L))
  
(* Tests for radixsort 
val [] = radixsort([])
val [1,2,3] = radixsort([1,2,3])
val [1,3,42,54,217,314,9001] = radixsort [1,3,42,9001,314,217,54] *)

(* Task 6.1 *)
(* make : int -> int list -> string list option *)
(* REQUIRES: n > 0 *)
(* ENSURES: make(x)(L) returns SOME(y) if some combination of L can produce x where y is a list of "*" and "+" that indicate how to combine the integers of L to produce x and NONE otherwise *)
fun make (n : int) ([] : int list) : string list option = NONE
  | make (n : int) (x::L : int list) : string list option =
      if (n = x) andalso (L = [])
        then SOME []
        else 
          case (make(n-x) L, make(n div x) L, n mod x = 0) of
              (SOME e, _, _) => SOME ("+"::e)
            | (_, SOME e, true) => SOME ("*"::e)
            | _ => NONE
        
(* Tests for make *)
val SOME ["+","*"] = make 7 [1,2,3]
val SOME ["+"] = make 8 [2,6]
val SOME ["*"] = make 9 [3,3]
val SOME [] = make 5 [5]
val NONE = make 4 [1,3,6]
val NONE = make 3 []

(* Task 6.2 *)
(* make_C : int -> int list -> (string list -> 'a) -> (unit -> 'a) -> 'a *)
(* REQUIRES: n > 0, f and k are total functions *)
(* ENSURES: make_C(x)(L)(f)(k) returns f applied to the string list of operations if there is some combination of L that produces x and k() otherwise *)
fun make_C (n : int) ([] : int list)
           (s : string list -> 'a) (k : unit -> 'a) : 'a = k()
  | make_C (n : int) (x::L : int list)
           (s : string list -> 'a) (k : unit -> 'a) : 'a =
      if (n = x) andalso (L = [])
        then s([])
        else
          case n mod x of
              0 => make_C (n-x) L (fn x => s("+"::x)) (fn () => make_C (n div x) L (fn x => s("*"::x)) k)
            | _ => make_C (n-x) L (fn x => s("+"::x)) k

(* Tests for make_C *)
val rec f = fn [] => "" | x::L => x ^ f(L)
val k = fn L => "NONE"
val "+*" = make_C 7 [1,2,3] f k
val "+" = make_C 8 [2,6] f k
val "*" = make_C 9 [3,3] f k
val "" = make_C 5 [5] f k
val "NONE" = make_C 4 [1,3,6] f k
val "NONE" = make_C 3 [] f k
