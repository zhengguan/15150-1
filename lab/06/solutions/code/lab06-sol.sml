(* -------------------- Task 3 -------------------- *)

(* fold code for your reference *)

(*
 * foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
 * ENSURES: Given a function f, an identity e, and a list l = [a_0 ... a_n],
 * foldr f e l evaluates to f(a_0, f(a_1, f(a_2, ... f(a_n, e) ... ))), or e
 * if the list is empty. foldr stands for "fold right"
 *)
fun foldr (f : 'a * 'b -> 'b) (e : 'b) (l : 'a list) : 'b =
    case l of
      []  => e
    | x::xs => f (x, foldr f e xs)

(*
 * sum : int list -> int
 * ENSURES: sum l evaluates to the sum of the elements in l
 *)
fun sum (l : int list) : int =
    case l of
      []  => 0
    | x::xs => x + (sum xs)

(*
 *sum' : int list => int
 * ENSURES: sum' l evaluates to the sum of the elements in l
 *)
fun sum' (l : int list) : int = foldr (fn (x,y) => x + y) 0 l


(* Task 3.2 *)
val even = (fn x => x mod 2 = 0)

(*
 * exists : ('a -> bool) -> 'a list -> bool
 * REQUIRES: true
 * ENSURES: true iff. there is at least one element a in
 * l such that p a == true
 *)
fun exists (p : 'a -> bool) (l : 'a list) : bool =
    foldr (fn (a,b) => p a orelse b) false l

(* Tests *)
val true = exists even [2]
val true = exists even [1,2,3]
val false = exists even []
val false = exists even [1,1,1]

(* forall : ('a -> bool) -> 'a list -> bool
 * REQUIRES: true
 * ENSURES: true iff. p a == true for every a in l
 *)
fun forall (p : 'a -> bool) (l : 'a list) : bool =
    foldr (fn (a,b) => p a andalso b) true l

(* Tests *)
val true = forall even [2,2,2]
val true = forall even []
val false = forall even [2,4,8,11]
val false = forall even [1,5,0]

(* -------------------- Task 4 -------------------- *)

(* Polymorphic tree datatype representing a tree with data ONLY at leaves *)
datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* Task 4.1 *)
(*
 * treeFilter : ('a -> bool) -> 'a tree -> 'a option tree
 * REQUIRES: true
 * ENSURES: treeFilter p t evaluates to a tree T such that for each
 *          element x in t, if p x, then the value of the node at 
 *          that point in T is SOME(x), and if not, then the value of 
 *          the node is NONE.
 *)
fun treeFilter (p : 'a -> bool) (Empty) : 'a option tree = Empty
  | treeFilter (p) (Node (l, x, r)) =
    let
      val newX = if p x then SOME x else NONE
    in
      Node (treeFilter p l, newX, treeFilter p r)
    end

(* Tests *)
val p = (fn x => x = 3)
val Empty = treeFilter p Empty
val Node(Empty,NONE,Empty) = treeFilter p (Node(Empty,4,Empty))
val Node(Empty,SOME(3),Empty) = treeFilter p (Node(Empty,3,Empty))
val Node(Node(Empty,NONE,Empty),SOME(3),Empty) 
        = treeFilter p (Node(Node(Empty,524,Empty),3,Empty))



(* Task 4.2 *)
(*
 * treexists : ('a -> bool) -> 'a tree -> 'a option
 * REQUIRES: true
 * ENSURES: treexists p t evaluates to SOME e, where e is an element of t
 *          that satisfies p; if no such element exists, returns NONE.
 *)
fun treexists (p : 'a -> bool) (Empty : 'a tree) : 'a option = NONE
  | treexists (p) (Node (l, x, r)) =
    if p x then SOME x
    else case treexists p l of NONE => treexists p l
                             | s => s

(* Tests *)
val p = List.null;


(* Task 4.3 *)
(*
 * treeAll : ('a -> bool) -> 'a tree -> bool
 * REQUIRES:
 * ENSURES: *)
fun treeAll (p : 'a -> bool) (Empty : 'a tree) : bool = true
  | treeAll (p) (Node (l, x, r)) : bool =
    (p x) andalso (treeAll p l) andalso (treeAll p r)

(* Task 4.4 *)
(*
 * treeAll' : ('a -> bool) -> 'a tree -> bool
 * REQUIRES:
 * ENSURES:
 *)
fun treeAll' (p : 'a -> bool) (t : 'a tree) : bool =
    let
      val filtered = treeFilter p t
      val existsNot = treexists (fn (SOME _) => false
                                  | (NONE) => true) filtered
    in
      case existsNot
        of NONE => true
         | _ => false
    end

(* Tests *)
val true = treeAll' (fn x => x)
           (Node (Node (Empty, true, Empty), true, Empty))
val false = treeAll' (fn x => x)
           (Node (Node (Empty, false, Empty), true, Empty))
val true = treeAll' (fn n => n mod 2 = 0)
           (Node (Node (Empty, 312, Empty), 150, Node (Empty, 210, Empty)))

(* Task 4.6 *)
(*
 * onlyEvenTrees : (int tree) tree -> (int tree option) tree
 * REQUIRES:
 * ENSURES:
 *)
fun onlyEvenTrees (t : int tree tree) : (int tree option) tree =
    treeFilter (treeAll (fn n => (n mod 2 = 0))) t

(* Tests *)
val Node (Empty, SOME (Node (Empty, 150, Empty)), Empty) =
    onlyEvenTrees (Node (Empty, Node (Empty, 150, Empty), Empty))
val Node (Empty, NONE, Empty) =
    onlyEvenTrees (Node (Empty, Node (Empty, 251, Empty), Empty))
val Node (
      Node ( Empty, SOME (Node (Node (Empty, 2, Empty), 4, Empty)), Empty),
      NONE,
      Node (Empty, NONE, Empty)) =
    onlyEvenTrees (
      Node (
        Node (Empty, Node (Node (Empty, 2, Empty), 4, Empty), Empty),
        Node (Empty, 9, Empty),
        Node (Empty, Node (Empty, 16, Node (Empty, 1, Empty)), Empty)))

(* Task 4.7 *)
(*
 * safetree : int tree -> int option tree
 * REQUIRES: true
 * ENSURES: safetree t evaluates to a tree equivalent to t with each Leaf x
 * replaced by NONE where x = 0 and SOME x otherwise.
 *)
fun safetree (t : int tree) : int option tree =
    treeFilter (fn x => x <> 0) t

(* Tests *)
val Node (Empty, SOME 1, Empty) = safetree (Node (Empty, 1, Empty))
val Node (Empty, NONE, Empty) = safetree (Node (Empty, 0, Empty))
val Node (Node (Empty, SOME ~1, Empty), NONE, Node (Empty, SOME 1, Empty)) =
    safetree (Node (Node (Empty, ~1, Empty), 0, Node (Empty, 1, Empty)))

(* Task 5 *)

datatype 'a narytree = Empty
                     | Leaf of 'a
                     | Node of 'a narytree list

(* Task 5.1 *)
(* REQUIRES: a > 0, n >= 0
 * ENSURES: an narytree with depth n and a^n leaves each containing 42 *)
fun fuller (0, a) = Leaf 42
  | fuller (n, a) = Node (List.tabulate(a, fn _ => fuller (n-1, a)))

(* Tests *)
val Leaf 42 = fuller (0, 3)
val Leaf 42 = fuller (0, 7)
val Node [Leaf 42, Leaf 42, Leaf 42] = fuller (1, 3)
val Node [Node [Leaf 42, Leaf 42, Leaf 42],
          Node [Leaf 42, Leaf 42, Leaf 42],
          Node [Leaf 42, Leaf 42, Leaf 42]] = fuller (2, 3)

(* Task 5.2 *)
(*
 * narytreemap : ('a -> 'b) -> 'a naryTree -> 'b naryTree
 * REQUIRES: true
 * ENSURES: narytreemap f t evaluates to a new naryTree for which each Leaf a
 * has been replaced with Leaf (f a)
 *)
fun narytreemap (f : 'a -> 'b) (t : 'a narytree) : 'b narytree =
    case t of
      Empty => Empty
    | Leaf a => Leaf (f a)
    | Node l => Node (map (narytreemap f) l)

(* Tests *)
val addOne = fn x => x + 1
val mtest0 = Node [Leaf 0, Leaf 1, Leaf 2]
val mtest1 = Node [Empty, Leaf 7, Empty]

val Node [Leaf 1, Leaf 2, Leaf 3] = narytreemap addOne mtest0
val Node [Empty, Leaf 8, Empty] = narytreemap addOne mtest1

(* Task 5.3 *)
(*
 * narytreereduce : ('a * 'a -> 'a) -> 'a -> 'a naryTree -> 'a
 * REQUIRES: true
 * ENSURES: narytreefold f b t evaluates to a value of type 'a resulting from
 * applying the associative operator f to every 'a in t, using b as the identity
 *)
fun narytreereduce (g : 'a * 'a -> 'a) (b : 'a) (t : 'a narytree) : 'a =
    case t of
      Empty => b
    | Leaf a => a
    | Node l => foldr g b (map (narytreereduce g b) l)

(* Tests *)
val rtest0 = Node [Node [Leaf 8, Leaf 1, Leaf 6],
                   Node [Leaf 3, Leaf 5, Leaf 7],
                   Node [Leaf 4, Leaf 9, Leaf 2]]
val rtest1 = Node [Node [Leaf 2, Leaf 5],
                   Node [Leaf 3, Leaf 4]]

val 45 = narytreereduce op+ 0 rtest0
val 120 = narytreereduce op* 1 rtest1


(* Task 5.4 *)
(*
 * narytreemapfold : ('a -> 'b) -> ('b * 'b -> 'b) -> 'b -> 'a naryTree -> 'b
 * REQUIRES: true
 * ENSURES: narytreefold f b t evaluates to a value of type 'a resulting from
 * applying the associative operator f to every 'a in t, using b as the identity
 *)
fun narytreemapreduce (f : 'a -> 'b) (g : 'b * 'b -> 'b) (b : 'b)
                    (t : 'a narytree) : 'b =
    narytreereduce g b (narytreemap f t)

(* Tests *)
val mrtest0 = Node [Node [Leaf 1, Leaf 2, Leaf 3],
                    Node [Leaf 4, Leaf 5, Leaf 6]]
val mrtest1 = Node [Node [Leaf 72, Leaf 69, Leaf 76],
                    Node [Leaf 76, Leaf 79, Leaf 33]]
val 42 = narytreemapreduce (fn x => x * 2) op+ 0 mrtest0
val "HELLO!" =
    narytreemapreduce (fn i => Char.toString (chr i)) op^ "" mrtest1
