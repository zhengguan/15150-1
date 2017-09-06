  (* Defining Trees *)
datatype tree =
    Node of tree * int * tree
  | Empty

datatype rtree =
    rNode of rtree * real * rtree
  | rEmpty

(* Real trees cannot be compared directly (because reals cannot be compared
 * directly).  Use this function to check if two rtrees are the same, using
 * a small positive epsilon. *)  
fun treeCompare(epsilon : real, rEmpty : rtree, rEmpty : rtree) = true
  | treeCompare(epsilon, rNode(a, x, b), rNode(c, y, d)) =
  		(y < (x + epsilon)) andalso (y > (x - epsilon)) andalso
  		treeCompare(epsilon, a, c) andalso treeCompare(epsilon, b, d)
  | treeCompare(_, _, _) = false


(* TASK 2 *)

(* fastfib : int -> int *) 
(* REQUIRES: n>=1 *)
(* ENSURES: fastfib(n) evaluates to the nth Fibonacci number *)
fun fastfib (1 : int) : int = 1
  | fastfib (2 : int) : int = 1
  | fastfib (n : int) : int =
    case (n mod 2) of
        0 => fastfib(n div 2) * (2 * fastfib(n div 2 + 1) - fastfib(n div 2))
      | _ => (fastfib(n div 2 + 1) * fastfib(n div 2 + 1)) + (fastfib(n div 2) * fastfib(n div 2))

(* Tests for fastfib *)
val 1 = fastfib(1)
val 1 = fastfib(2)
val 5 = fastfib(5)
val 267914296 = fastfib(42)


(* TASK 3 *)

(* pow : real * int -> real *)
(* REQUIRES: k >= 0 *)
(* ENSURES: pow(r, k) evaluates to r to the power k *)
fun pow (r : real, 0 : int) : real = 1.0
  | pow (r : real, k : int) : real = r * pow(r, k-1)

(* Test for pow *)
val true = Real.==(pow(2.0, 2), 4.0)
val true = Real.==(pow(2.0, 3), 8.0)
val true = Real.==(pow(4.0, 2), 16.0)
val true = Real.==(pow(3.0, 0), 1.0)

(* partial : int * real -> real *)
(* REQUIRES: n >= 0 *)
(* ENSURES: partial(n, r) evaluates to Sn(r) *)
fun partial (0 : int, r : real) : real = 1.0
  | partial (1 : int, r : real) : real = r
  | partial (n : int, r : real) : real = pow(r, n) + partial(n-1, r)

(* Tests for partial *)
val true = Real.==(partial(3, 1.0/3.0), 13.0/27.0)
val true = Real.==(partial(2, 2.0), 6.0)
val true = Real.==(partial(0, 2.0), 1.0)

(* geometricTreeLevel : int * int * real -> real *)
(* REQIURES: n >= m >= 0 *)
(* ENSURES: geometricTreeLevel(n, m, r) returns the complete binary tree of size 2^(n+1-m)-1 with depth (n-m) with the read value at each node being Si(r) where i is the sum of depth of that node in the tree and m. *)
fun geometricTreeHelp (n : int, m : int, r : real) : rtree = 
    if n < m
      then rEmpty
      else 
        let
          val a = geometricTreeHelp(n, m+1, r)
        in
          rNode(a, partial(m, r), a)
        end

(* Tests for geometricTreeHelp *)
val true = treeCompare(0.0001, rNode(rNode(rEmpty,2.0,rEmpty), 1.0, rNode(rEmpty,2.0,rEmpty)), geometricTreeHelp(1, 0, 2.0))
val true = treeCompare(0.0001, rNode(rNode(rEmpty,1.0,rEmpty), 1.0, rNode(rEmpty,1.0,rEmpty)), geometricTreeHelp(1, 0, 1.0))
val true = treeCompare(0.0001, rNode(rNode(rEmpty,39.0,rEmpty), 12.0, rNode(rEmpty,39.0,rEmpty)), geometricTreeHelp(3, 2, 3.0))
val true = treeCompare(0.0001, rNode(rEmpty, 1.0, rEmpty), geometricTreeHelp(0, 0, 3.0))
val true = treeCompare(0.0001, rNode(rEmpty, 14.0, rEmpty), geometricTreeHelp(3, 3, 2.0))
val true = treeCompare(0.0001, rEmpty, geometricTreeHelp(2,3,4.0))

(* geometricTree : int * real -> real *)
(* REQIURES: n >= 0 *)
(* ENSURES: geometricTree(n, r) returns the complete binary tree of size 2^(n+1)-1 with depth n with the read value at each node being Si(r) where i is the depth of that node in the tree. *)
fun geometricTree (n : int, r : real) : rtree = geometricTreeHelp(n, 0, r)

(* Tests for geometricTreeHelp *)
val true = treeCompare(0.0001, rNode(rNode(rEmpty,2.0,rEmpty), 1.0, rNode(rEmpty,2.0,rEmpty)), geometricTree(1, 2.0))
val true = treeCompare(0.0001, rNode(rNode(rEmpty,1.0,rEmpty), 1.0, rNode(rEmpty,1.0,rEmpty)), geometricTree(1, 1.0))
val true = treeCompare(0.0001, rNode(rNode(rNode(rEmpty,12.0,rEmpty), 3.0, rNode(rEmpty,12.0,rEmpty)), 1.0, rNode(rNode(rEmpty,12.0,rEmpty), 3.0, rNode(rEmpty,12.0,rEmpty))), geometricTree(2, 3.0))
val true = treeCompare(0.0001, rNode(rEmpty, 1.0, rEmpty), geometricTree(0, 3.0))


(* TASK 4 *)

(* part : int * int list -> int list * int list *)
(* REQUIRES true *)
(* ENSURES part(x, L) = a pair of lists (A,B) such that A consists of the items in L that are less than x and B consists of the items in L that are greater than or equal to x. *)
fun part (p : int, [] : int list) : int list * int list = ([], [])
  | part (p : int, x::L :int list) : int list * int list = 
    let 
      val (a, b) = part(p, L)
    in
      if  x < p
        then (x::a, b) 
        else (a, x::b)
    end

(* Tests for part *)
val ([2,1], [3,4]) = part(3, [3,2,4,1])
val ([2,1,3], []) = part(4, [2,1,3])
val ([], [2,3,4]) = part(1, [2,3,4])
val ([], []) = part(5, [])

(* quicksort : int list -> int sort *)
(* REQUIRES: true *)
(* quicksort(L) evaluates to a sorted permutation of L *)
fun quicksort ([] : int list) : int list = []
  | quicksort (x::L :int list) : int list = 
    let 
      val (a, b) = part(x, L)
    in
      quicksort(a) @ x::quicksort(b)
    end
    
(* Tests for quicksort *)
val [1,2,3] = quicksort([2,3,1])
val [1,2,3,4] = quicksort([4,3,2,1])
val [] = quicksort([])


(* TASK 5 *)

(* traver : tree * int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: traver(T, L) = trav(T) @ L *)
fun traver (Empty : tree, L : int list) : int list = L
  | traver (Node(t1, x, t2) : tree, L : int list) : int list = traver(t1, x::traver(t2, L))

(* Tests for traver *)
val [2,1,3,4,5] = traver(Node(Node(Empty, 2, Empty), 1, Node(Empty, 3, Empty)), [4,5])
val [1,3,4,5] = traver(Node(Empty, 1, Node(Empty, 3, Empty)), [4,5])
val [2,1,4,5] = traver(Node(Node(Empty, 2, Empty), 1, Empty), [4,5])
val [2,1,3] = traver(Node(Node(Empty, 2, Empty), 1, Node(Empty, 3, Empty)), [])
val [4,5] = traver(Empty, [4,5])
val [] = traver(Empty, [])


(* TASK 7 *)

(* treecompare : tree * tree -> order *)
(* REQUIRES: true *)
(* ENSURES: evalutes to a value fo type order based on which tree has a larger value at the root node. *)
fun treecompare (Empty : tree, Empty : tree) : order = EQUAL
  | treecompare (Empty : tree, T2 : tree) : order = LESS
  | treecompare (T1 : tree, Empty : tree) : order = GREATER
  | treecompare (Node(t1, x, t2) : tree, Node(t3, y, t4) : tree) : order = Int.compare(x, y)

(* Test for treecompare *)  
val GREATER = treecompare(Node(Empty, 4, Empty), Node(Node(Empty, 7, Empty), 3, Empty))
val LESS = treecompare(Node(Empty, 3, Empty), Node(Node(Empty, 7, Empty), 4, Empty))
val EQUAL = treecompare(Node(Empty, 4, Empty), Node(Node(Empty,3, Empty), 4, Empty))
val GREATER = treecompare(Node(Empty, 4, Empty), Empty)
val LESS = treecompare(Empty, Node(Empty, 3, Empty))
val EQUAL = treecompare(Empty, Empty)

(* makeSwapDown : int * tree -> bool * int * tree *)
(* REQUIRES: true *)
(* ENSURES: makeSwapDown(x, T) swaps x and the root of T if x > value(T) and then evaluates to a true if the swap took place and false otherwise as well as the value of x and T after the possible swap. *)
fun makeSwapDown(x : int, Empty : tree) : bool * int * tree = (false, x, Empty)
  | makeSwapDown(x : int, Node(t1, y, t2)) : bool * int * tree =
    case Int.compare(x, y) of
        GREATER => (true, y, Node(t1, x, t2)) 
      | _ => (false, x, Node(t1, y, t2))

(* Tests for makeSwapDown *)
val (true, 1, Node(Node(Empty, 2, Empty), 4, Node(Empty, 3, Empty))) = makeSwapDown(4, Node(Node(Empty, 2, Empty), 1, Node(Empty, 3, Empty)))
val (false, 1, Node(Node(Empty, 2, Empty), 4, Node(Empty, 3, Empty))) = makeSwapDown(1, Node(Node(Empty, 2, Empty), 4, Node(Empty, 3, Empty)))
val (false, 2, Empty) = makeSwapDown(2, Empty)

(* swapDown : tree -> tree *)
(* REQUIRES the subtrees of t are both minheaps *)
(* ENSURES swapDown(t) = if t is Empty or all of t’s immediate children are empty then just return t, otherwise returns a minheap which contains exactly the elements in t. *)
fun swapDown (Empty : tree) : tree = Empty
  | swapDown (Node(Empty, x, Empty) : tree) : tree = Node(Empty, x, Empty)
  | swapDown (Node(t1, x, t2) : tree) : tree = 
    let
      val (a, b, c) = makeSwapDown(x, t1)
      val (d, e, f) = makeSwapDown(x, t2)
    in
      if a
        then swapDown(Node(swapDown(c), b, t2))
        else
          if d
            then swapDown(Node(t1, e, swapDown(f)))
            else Node(t1, x, t2)
    end

(* Tests for swapDown *)
val Node(Node(Empty, 2, Empty), 1, Node(Empty, 3, Empty)) = swapDown(Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty)))
val Node(Node(Empty, 6, Empty), 4, Node(Empty, 5, Empty)) = swapDown(Node(Node(Empty, 6, Empty), 5, Node(Empty, 4, Empty)))
val Node(Node(Node(Empty, 4, Empty), 2, Empty), 1, Node(Empty, 3, Empty)) = swapDown(Node(Node(Node(Empty, 2, Empty), 1, Empty), 4, Node(Empty, 3, Empty)))
val Node(Node(Node(Empty, 4, Empty), 2, Empty), 1, Node(Empty, 3, Empty)) = swapDown(Node(Node(Node(Empty, 4, Empty), 1, Empty), 2, Node(Empty, 3, Empty)))
val Node(Node(Node(Empty, 3, Empty), 4, Empty), 1, Node(Empty, 2, Empty)) = swapDown(Node(Node(Node(Empty, 3, Empty), 4, Empty), 2, Node(Empty, 1, Empty)))
val Empty = swapDown(Empty)

(* heapify : tree -> tree *)
(* REQUIRES: true *)
(* ENSURES: heapify(T) evaluates to a minheap with exactly the elements of T. *)
fun heapify (Empty : tree) : tree = Empty
  | heapify (Node(t1, x, t2) : tree) : tree = swapDown(Node(heapify(t1), x, heapify(t2)))
  
(* Test for heapify *)
(* Tests for makeSwapDown *)
val Node(Node(Empty, 2, Empty), 1, Node(Empty, 3, Empty)) = heapify(Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty)))
val Node(Node(Empty, 6, Empty), 4, Node(Empty, 5, Empty)) = heapify(Node(Node(Empty, 6, Empty), 5, Node(Empty, 4, Empty)))
val Node(Node(Node(Empty, 4, Empty), 2, Empty), 1, Node(Empty, 3, Empty)) = heapify(Node(Node(Node(Empty, 2, Empty), 1, Empty), 4, Node(Empty, 3, Empty)))
val Node(Node(Node(Empty, 4, Empty), 2, Empty), 1, Node(Empty, 3, Empty)) = heapify(Node(Node(Node(Empty, 4, Empty), 1, Empty), 2, Node(Empty, 3, Empty)))
val Node(Node(Node(Empty, 4, Empty), 3, Empty), 1, Node(Empty, 2, Empty)) = heapify(Node(Node(Node(Empty, 3, Empty), 4, Empty), 2, Node(Empty, 1, Empty)))
val Empty = heapify(Empty)
