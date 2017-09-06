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
(* Fast Fib *)

(* fastfib : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: fastfib(n) evaluates to the n'th Fibonacci number *)
fun fastfib(n : int) : int =
    let

      (* REQUIRES: n >= 0 *)
      (* ENSURES: fibhelp(n) = (fib(n+1),fib(n)) *)
      fun fibhelp (n : int) : int*int =
          case n of
            0 => (1,0)
          | _ =>
            let
              val (kp1, k) = fibhelp(n div 2)
              val (f_np1, f_n) = (kp1 * kp1 + k * k, k * (2 * kp1 - k))
            in
              if n mod 2 = 0 then
                (f_np1, f_n)
              else
                (f_np1 + f_n, f_np1)
            end

      val (_,b) = fibhelp n
    in
      b
    end

val 0 = fastfib(0)
val 5 = fastfib(5)
val 1 = fastfib(1)
val 3 = fastfib(4)

(* TASK 3 *)
(* Geometric Tree *)

(* GeomHelp : int*real*real*real -> rtree *)
(* REQUIRES: i >= 0 *)
(* ENSURES: GeomHelp(i, r, acc, first) returns a tree of depth i such that the
 *   value of all nodes at depth i is the i'th partial sum of the geometric
 *  series with first term first + acc, and common ratio r
 *)
fun GeomHelp (0 : int, r : real, acc : real, first : real) : rtree = rEmpty
  | GeomHelp (i, r, acc, first) =
    let
      val subtree = GeomHelp(i - 1, r, r * (acc + first), first)
    in
      rNode(subtree, acc + first, subtree)
    end

val true = treeCompare(0.01, rNode(rEmpty, 1.0, rEmpty),
                       GeomHelp(1, 0.5, 0.0, 1.0))
val true = treeCompare(0.01, rNode(rEmpty, 1.5, rEmpty),
                       GeomHelp(1, 1.5, 0.0, 1.5))
val true = treeCompare(0.01, rEmpty, GeomHelp(0, 1.0, 0.0, 4.0))
val true = treeCompare(0.01, rEmpty, GeomHelp(0, 7981.98237, 0.0, 4.0))


(* geometricTree : int*real -> rtree *)
(* REQUIRES: d>= 0 *)
(* ENSURES: geometricTree(d, r) returns a tree of depth d where all nodes
 *  at level k have value equal to the k'th element of the geometric
 *  series with first term 1.0 and common ratio r. *)
fun geometricTree (d : int, r : real) : rtree = GeomHelp(d, r, 0.0, 1.0)
fun geometricTreeB (d : int, r : real) : rtree = GeomHelp(d + 1, r, 0.0, 1.0)

val true = treeCompare(0.01, rNode(rEmpty, 1.0, rEmpty), geometricTree(1, 0.5))
val true = treeCompare(0.01, rEmpty, geometricTree(0, 4.0))


(* TASK 4 *)
(* Quicksort *)

(* part : int * int list -> int list * int list *)
(* REQUIRES: true *)
(* ENSURES: part(p, L) returns (a, b) such that a @ b is a permutation of
 *   L, where all elements in a are less than p, and all elements in b are
 *   greater than or equal to p *)
fun part (p : int, L : int list) =
    case L of
      [] => ([], [])
    | x :: L' =>
      let
        val (Less, Greater) = part(p, L')
      in
        case Int.compare(x,p)
         of LESS => (x :: Less, Greater)
          | _ => (Less, x :: Greater)
      end

val ([2, 3, 1], [4, 5, 6, 4]) = part(4, [4, 5, 2, 3, 6, 1, 4])
val ([],[]) = part(0, [])

(* quicksort : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: quicksort(l) evaluates to a sorted permutation of L *)
fun quicksort ([] : int list) = []
  | quicksort (x::xs) =
    let
      val (L, GE) = part(x, xs)
    in
      quicksort L @ x :: quicksort GE
    end

val ([1, 2, 3, 4, 4, 5, 6]) = quicksort([4, 5, 2, 3, 6, 1, 4])
val [] = quicksort []
val [~1] = quicksort [~1]

(* TASK 5 *)
(* Tree Traversal *)

(* traverse : itree * int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: traver(t, L) evaluates to the traversal list of t @ L *)
fun traver(Empty : tree, L : int list) = L
  | traver(Node(a, x, b), L) = traver(a, x::traver(b, L))


val [3, 7, 9, 20] = traver(Node(Node(Empty, 3, Empty), 7, Empty), [9, 20])
val [1, 2, 3]= traver(Empty, [1,2,3])

(* traverse : itree -> int list *)
(* REQUIRES: true *)
(* ENSURES: traverse(t) returns the traversal list of t *)
fun traverse (t : tree) : int list = traver(t, [])

val [3, 7] = traverse(Node(Node(Empty, 3, Empty), 7, Empty))
val [] = traverse (Empty)


(* TASK 7 *)
(* Minheap Sorting *)

(* Comparing two trees *)

(* treecompare : tree * tree -> order *)
(* REQUIRES: true *)
(* ENSURES: Evaluates to the integer comparison of the values at each
 *   argument's top node or leaf *)
fun treecompare(Empty : tree, Empty : tree) : order = EQUAL
  | treecompare(Empty, _    ) = GREATER
  | treecompare(_    , Empty) = LESS
  | treecompare(Node(_, i, _), Node(_, j, _)) = Int.compare(i, j)

val EQUAL = treecompare(Node(Empty, 3, Empty),
                        Node(Node(Empty, 5, Empty), 3, Empty))
val GREATER = treecompare(Node(Empty, 5, Empty), Node(Empty, 2, Empty))
val LESS = treecompare(Node(Empty, 3, Empty), Node(Empty, 4, Empty))

(* Given a tree with minheap children, makes the tree a minheap *)

(* swapDown : tree -> tree *)
(* REQUIRES: a, b are both minheaps *)
(* ENSURES: swapDown(t) is a minheap *)
fun swapDown(Empty : tree) : tree = Empty
  | swapDown(T as Node(a, x, b)) =
    let
        val (smallTree, bigTree) = case treecompare(a, b) of GREATER => (b, a)
                                                           | _       => (a, b)
    in
        case smallTree of
          Empty => Node(smallTree, x, bigTree)
        | Node(c, y, d) =>
          (case treecompare(T, smallTree) of
             GREATER => Node(swapDown(Node(c, x, d)), y, bigTree)
           | _ => T)
    end

val Node(Node(Empty, 3, Empty), 2, Node(Empty, 4, Empty)) =
    swapDown(Node(Node(Empty, 4, Empty), 3, Node(Empty, 2, Empty)))
val Empty = swapDown(Empty)

(* Given an arbitrary tree, makes the tree a minheap *)

(* heapify : tree -> tree *)
(* REQUIRES: true *)
(* ENSURES: heapify(t) is a minheap containing all and only the elements of t *)
fun heapify(Empty : tree) : tree = Empty
  | heapify(Node(t1, i, t2)) = swapDown(Node(heapify t1, i, heapify t2))


val Node(Node(Empty, 4, Empty), 2, Empty) =
    heapify(Node(Node(Empty, 2, Empty), 4, Empty))
val Empty = heapify Empty
