use "../../code/lib.sml";

(* Task 2.1
 * zip : string list * int list -> (string * int) list
 * REQUIRES: true
 * ENSURES:  zip(L1, L2) returns a list such that the nth element in
 *           the first list is paired with the nth element from the second list.
 *           Truncates the returned list based on the shortest input list.
 *
 * Examples:
 *  zip(["a","b"],[1,2]) = [("a",1),(2,"b")]
 *  zip(["a"],[1,2,3]) = [("a",1)]
 *  zip(["a","b","c","d"],[1,2],) = [("a",1),("b",2)]
 *)
fun zip ([] : string list, _ : int list) : (string * int) list = []
  | zip (_, []) = []
  | zip (x::xs, y::ys) = (x,y) :: zip (xs,ys)

(* Tests for zip *)
val [("a",1), ("b",2)] = zip (["a","b"], [1,2])
val [("a",1)] = zip (["a"],[1,2,3])
val [("a",1), ("b",2)] = zip (["a","b","c","d"],[1,2])


(* Task 2.2
 * unzip : (string * int) list -> string list * int list
 * REQUIRES: true
 * ENSURES:  unzip(L) returns a pair of lists such that the first list is
 *           the list of first elements in the pairs of L, and the second
 *           list is the list of second elements.
 * Examples:
 *  unzip[("a", 1),("b", 1)] = (["a","b"], [1,2])
 *  unzip[("dragon", 42),("llama", 54),("muffin", 76)] =
 *    (["dragon","llama","muffin"], [42,54,76])
 *)
fun unzip ([] : (string*int) list) : string list * int list = ([],[])
  | unzip ((x1,x2)::xs) =
    let
      val (l1,l2) = unzip xs
    in
      (x1::l1, x2::l2)
    end

(* Tests for unzip *)
val (["a","b"],[1,2]) = unzip [("a",1), ("b",2)]
val (["dragon","llama","muffin"], [42,54,76]) =
  unzip [("dragon",42), ("llama",54), ("muffin",76)]


(* Task 3.1 *)
(* runWith : int * int list  -> int list * int list
 * REQUIRES: true
 * ENSURES:  runWith(x,L) returns (repeated, tail), where repeated is an int
 *           list with all of the copies of x from the front of L, and tail,
 *           which is all of the remaining elements.
 *
 * Examples:
 *  runWith (1, [1, 1, 2]) = ([1, 1], [2])
 *  runWith (1, [1,1,1,1,5,2]) = ([1,1,1,1],[5,2])
 *  runWith (2, [1,1,1,1,5,2]) = ([], [1,1,1,1,5,2])
 *)
fun runWith (_:int, [] : int list) : int list * int list = ([], [])
  | runWith (x, y::L) =
    if x = y then
      let
        val (repeats, tail) = runWith(x, L)
      in
        (x::repeats, tail)
      end
    else
      ([], y::L)

(* Tests for lasHelp *)
val ([1], []) = runWith (1, [1])
val ([1, 1], [2]) = runWith (1, [1, 1, 2])
val ([1,1,1,1],[5,2]) = runWith (1, [1,1,1,1,5,2])
val ([], [1,1,1,1,5,2]) = runWith (2, [1,1,1,1,5,2])

(* lookSay : int list -> (int*int) list
 * REQUIRES: true
 * ENSURES:  lookSay(L) counts the length of runs of elements in L,
 *           then returns that count followed by the element, for each run,
 *           in order.
 * Examples:
 *  lookSay ([1]) ==> [(1,1)]
 *  lookSay ([1,1,2,1,3]) ==> [(2,1),(1,2),(1,1),(1,3)]
 *  lookSay [] ==> [] (we choose nil for ease of implementation)
*)
fun lookSay ([] : int list) : (int*int) list = []
  | lookSay (x::L) =
    let
      val (repeat, tail) = runWith(x, x::L)
    in
      ((length repeat),x) :: (lookSay tail)
    end

(* Tests for look_and_say *)
val [(1,1)] = lookSay [1]
val [(2,1),(1,2),(1,1),(1,3)] = lookSay [1,1,2,1,3]
val [] = lookSay []



(* Functions for the tasks in section 4 *)
(* flatten : (int*int) list -> int list
 * REQUIRES: true
 * ENSURES:  flatten(L) returns a list of length 2L, where the elements
 *           in each pair are "flattened" into the result list.
 * Examples:
 *  flatten [] ==> []
 *  flatten ([(1,1),(2,3)]) ==> [1,1,2,3]
 *  flatten ([(5,3),(1,7)]) ==> [5,3,1,7]
 *)
fun flatten ([] : (int*int) list) : int list = []
  | flatten ((x,y)::L) = x::y::(flatten L)

val [] = flatten []
val [1,1,2,3] = flatten ([(1,1),(2,3)])
val [5,3,1,7] = flatten ([(5,3),(1,7)])


(* Functions for the tasks in section 4 *)
(* prefixSum : int list -> int list
 * REQUIRES: true
 * ENSURES:  prefixSum(L) returns a list whose i-th element is the sum of
 *           the first i elements of L, for i = 1,..., length(L).
 * Examples:
 *  prefixSum nil ==> nil
 *  prefixSum (1::2::3::nil) ==> 1::3::6::nil
 *  prefixSum (5::3::1::nil) ==> 5::8::9::nil
 *)
fun prefixSum ([] : int list) : int list = []
  | prefixSum  (x::L) = x :: addToEach (prefixSum L, x)

(* Tests for prefixSum *)
val [] = prefixSum []
val [1,3,6] = prefixSum [1,2,3]
val [5,8,9] = prefixSum [5,3,1]

(* prefixSumHelp : int list * int -> int list
 * REQUIRES: true
 * ENSURES:  prefixSumHelp(L, n) returns a list of the same length as L such
 *           that the i-th element is n more than the sum of the first i
 *           elements of L.
 * Examples:
 *  prefixSumHelp (nil, 7) ==> nil
 *  prefixSumHelp (1::2::3::nil, 2) ==> 3::5::8::nil
 *  prefixSumHelp (5::3::1::nil, 0) ==> 5::8::9::nil
 *)
fun prefixSumHelp ([] : int list, n : int) : int list = []
  | prefixSumHelp (x::L, n) = (n + x) :: prefixSumHelp (L, n + x)

(* Tests for prefixSumHelp *)
val [] = prefixSumHelp ([], 7)
val [3,5,8] = prefixSumHelp ([1,2,3], 2)
val [5,8,9] = prefixSumHelp ([5,3,1], 0)

(* prefixSumFast : int list -> int list
 * REQUIRES: true
 * ENSURES:  prefixSum(L) returns a list whose i-th element is the sum of
 *           the first i elements of L, for i = 1,..., length(L).
 * Examples:
 *  prefixSumFast nil ==> nil
 *  prefixSumFast (1::2::3::nil) ==> 1::3::6::nil
 *  prefixSumFast (5::3::1::nil) ==> 5::8::9::nil
 *)
fun prefixSumFast (l : int list) : int list =
    prefixSumHelp (l, 0)

(* Tests for prefixSumFast *)
val [] = prefixSumFast []
val [1,3,6] = prefixSumFast [1,2,3]
val [5,8,9] = prefixSumFast [5,3,1]

(* Functions for the tasks in section 5 *)
(* sublist : int * int * int list -> int list
 * REQUIRES: i >=0, k >= 0, i+k <= length(L)
 * ENSURES:  sublist(i,k,L) returns the k elements of L after and
 *           including the index i.  Lists are indexed from 0.
 * Examples:
 *  sublist(0,0,[50]) ==> []
 *  sublist(1,1,[50,100]) ==> [100]
 *  sublist(3,3, [1,2,3,4,5,6]) ==> [4,5,6]
 *)
fun sublist (i : int, k : int, [] : int list) : int list = []
  | sublist (0, 0, _ ) = []
  | sublist (0, k, x::xs) = x :: (sublist (0, k - 1, xs))
  | sublist (i,k,x::xs) = sublist (i - 1, k, xs)

(* tests for sublist *)
val [] = sublist(0, 0, [50])
val [100] = sublist(1, 1, [50,100])
val [4,5,6] = sublist(3, 3, [1,2,3,4,5,6])
val [3] = sublist(2,1, [1,2,3,4,5,6])


(* Functions for the tasks in section 7 *)

(* subsetSum : int list * int -> bool
 * REQUIRES: true
 * ENSURES:  subsetSum(L, s) returns true if there is a multi-subset of the
 *           elements in L summing to s, and false otherwise.
 * Examples:
 *  subsetSum ([], 0) ==> true
 *  subsetSum ([], 7) ==> false
 *  subsetSum ([2,3,2], 4) ==> true
 *  subsetSum ([2,4,6], 7) ==> false
 *)
fun subsetSum ([] : int list, s : int) : bool = s=0
  | subsetSum (x::L, s) = subsetSum (L, s - x) orelse subsetSum (L, s)

(* Tests for subsetSum *)
val true = subsetSum (nil, 0)
val false = subsetSum (nil, 7)
val true = subsetSum ([2,3,2], 4)
val false = subsetSum ([2,4,6], 7)
val true = subsetSum ([5,~2,1,~2],~3)

(* subsetSumCert : int list * int -> bool * int list
 * REQUIRES: true
 * ENSURES:  subsetSumCert(L,s) returns (true, S) if S is a multi-subset of the
 *           elements of L summing to s, and (false, nil) if no such S exists.
 * Examples:
 *  subsetSumCert (nil, 0) ==> (true, nil)
 *  subsetSumCert (nil, 7) ==> (false, nil)
 *  subsetSumCert (2::3::2::nil, 4) ==> (true, 2::2::nil)
 *  subsetSumCert (2::4::6::nil, 7) ==> (false, nil)
 *)
fun subsetSumCert (nil : int list, s : int) : bool * int list = (s=0,nil)
  | subsetSumCert (x::L, s) =
    case subsetSumCert (L, s - x) of
      (true, l1) => (true, x::l1)
    | (false, _) => subsetSumCert (L,s)

(* Tests for subsetSum_cred *)
val (true, []) = subsetSumCert ([], 0)
val (false, []) = subsetSumCert ([], 7)
val (true, [2,2]) = subsetSumCert ([2,3,2], 4)
val (false, []) = subsetSumCert ([2,4,6], 7)
