use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)

(* zip : string list * int list -> (string * int) list *)
(* REQUIRES: true *)
(* ENSURES: zip(l1, l2) pairs the nth elements from l1 and l2 *)
fun zip ([] : string list, l2 : int list) : (string * int) list = []
  | zip (l1 : string list, [] : int list) : (string * int) list = []
  | zip (x::l1 : string list, y::l2 : int list) : (string * int) list = (x,y)::zip(l1, l2)

(* Tests for zip *)
val [("a",5), ("b",1)] = zip(["a","b"], [5,1])
val [("a",5), ("b",1)] = zip(["a","b"], [5,1,2,1])
val [] = zip([], [5,1])
val [] = zip(["a","b"], [])

(* unzip : (string * int) list -> string list * int list *)
(* REQUIRES: true *)
(* ENSURES: zip(unzip(l)) = l *)
fun unzip ([] : (string * int) list) : string list * int list = ([], [])
  | unzip ((x,y)::l : (string * int) list) : string list * int list = 
    let
      val (a, b) = unzip(l)
    in
      (x::a, y::b)
    end

(* Tests for unzip *)
val (["a","b"], [5,1]) = unzip([("a",5), ("b",1)])
val ([], []) = unzip([])

(* ---------------------------------------------------------------------- *)
(* SECTION 3 *)

(* runWith : int * int list -> int list  * int list *)
(* REQUIRES: true *)
(* ENSURES: runWith(x,L) = (L1,L2) where L =  and every element of L1 is equal to x and L2 does not begin with x *)
fun runWith (x : int, [] : int list) : int list * int list = ([], [])
  | runWith (x : int, y::L : int list) : int list * int list = 
    if inteq(x, y)
      then
        let
          val (a, b) = runWith(y, L)
        in
          (x::a, b)
        end
      else ([], y::L)
    
(* Tests for runWith *)
val ([1], [2,3]) = runWith(1, [1, 2, 3])
val ([1,1], [2,3]) = runWith(1, [1,1,2,3])
val ([], [1,2,3]) = runWith(3, [1,2,3])

(* lookSay : int list -> (int * int) list *)
(* REQUIRES: true *)
(* ENSURES: lookSay(l) evalues to the look-and-say list of l *)
fun lookSay ([] : int list) : (int * int) list = []
  | lookSay (x::l : int list) : (int * int) list = 
    let 
      val (a, b) = runWith(x, x::l)
    in 
      (length(a), x)::lookSay(b)
    end

(* Tests for lookSay *)
val [(2,1), (1,2)] = lookSay([1,1,2])
val [(1,1), (2,2), (2,1)] = lookSay([1,2,2,1,1])
val [(3,1)] = lookSay([1,1,1])
val [(1,1), (1,2), (1,3)] = lookSay([1,2,3])
val [] = lookSay([])

(* flatten : (int * int) list -> int list *)
(* REQUIRES: true *)
(* ENSURES: flatten(l) evaluates to a "flattened" list of integers *)
fun flatten ([] : (int * int) list) : int list = [] 
  | flatten ((x,y)::l : (int * int) list) : int list = x::y::flatten(l)
  
(* Tests for flatten *)
val [1,2] = flatten([(1, 2)])
val [1,2,3,4,5,6] = flatten([(1, 2), (3, 4), (5, 6)])
val [] = flatten([])


(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* prefixSum : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSum(l) evaluates to the prefix sum of l in O(n^2) time *)
fun prefixSum ([] : int list) : int list = []
  | prefixSum (x::l : int list) : int list = x::addToEach(prefixSum(l), x)

(* Tests for prefixSum *)
val [1,3,6] =prefixSum [1,2,3]
val [5,8,9] =prefixSum [5,3,1]
val [] =prefixSum []

(* prefixSumHelp : int list * int -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSumHelp(l, x) adds x to the first element in l *)
fun prefixSumHelp ([] : int list, x : int) : int list = []
  | prefixSumHelp (y::l : int list, x : int) : int list = (x+y)::l

(* Tests for prefixSumHelp *)
val [5,2,3] =prefixSumHelp([1,2,3], 4)
val [10,3,1] =prefixSumHelp([5,3,1], 5)
val [] =prefixSumHelp([], 3)

(* prefixSumFast : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSumFast(l) evaluates to the prefix sum of l in O(n) time *)
fun prefixSumFast ([] : int list) : int list = []
  | prefixSumFast (x::l : int list) : int list = x::prefixSumFast(prefixSumHelp(l, x))

(* Tests for prefixSumFast *)
val [1,3,6] =prefixSumFast([1,2,3])
val [5,8,9] =prefixSumFast([5,3,1])
val [] =prefixSumFast([])

(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

(* sublist : int * int * int list -> int list *)
(* REQUIRES: 0<=i+j<=length(l) *)
(* ENSURES: sublist(i,j,l) returns the sublist of l starting at i of length j *)
fun sublist (i : int, j : int, [] : int list) : int list = []
  | sublist (i : int, 0 : int, x::l : int list) : int list = []
  | sublist (0 : int, j : int, x::l : int list) : int list = x::sublist(0, j-1, l)
  | sublist (i : int, j : int, x::l : int list) : int list = sublist(i-1, j, l)
  
(* Tests for sublist *)
val [2,3] = sublist(1, 2, [1,2,3])
val [1,2,3] = sublist (0, 3, [1,2,3,4])
val [3,4] = sublist(2, 2, [1,2,3,4,5])
val [] = sublist(0, 0, [1,2,3])
val [] = sublist(1, 0, [1,2,3,4])
val [] = sublist(4, 0, [1,2,3,4,5])
val [] = sublist(0, 0, [])

(* ---------------------------------------------------------------------- *)
(* SECTION 6 *)

(* subsetSum : int list * int -> int list *)
(* REQUIRES: true *)
(* ENSURES: subsetSum(l, s) returns true if there is a subset of l whose sum is s and false otherwise *)
fun subsetSum ([] : int list, 0 : int) : bool = true
  | subsetSum ([] : int list, s : int) : bool = false
  | subsetSum (x::l : int list, s : int) : bool = subsetSum(l, s-x) orelse subsetSum(l, s) 

(* Tests for subsetSum *)
val true = subsetSum([1,2,1,~6,10], 4)
val true = subsetSum([1,2,3], 0)
val false = subsetSum([1,3,6], 5)
val false = subsetSum([1,2,3], 8)
val true = subsetSum([], 0)
val false = subsetSum([], 2)

fun subsetSumCert ([] : int list, 0 : int) : bool * int list = (true, [])
  | subsetSumCert ([] : int list, s : int) : bool * int list = (false, nil)
  | subsetSumCert (x::l : int list, s: int) : bool * int list =
    let 
      val (a, b) = subsetSumCert(l, s-x)
      val (c, d) = subsetSumCert(l, s)
    in
      if a
        then (true, x::b)
        else
          if c
            then (true, d)
            else (false, nil)
    end
    
(* Tests for subsetSumCert *)
val (true, [1,2,1]) = subsetSumCert([1,2,1,~6,10], 4)
val (true, []) = subsetSumCert([1,2,3], 0)
val (false, nil) = subsetSumCert([1,3,6], 5)
val (false, nil) = subsetSumCert([1,2,3], 8)
val (true, []) = subsetSumCert([], 0)
val (false, nil) = subsetSumCert([], 2)
