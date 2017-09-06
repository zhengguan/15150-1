(* ---------------------------------------------------------------------- *)
(* Functions provided by the course staff. *)

(* max : int * int -> int
 * REQUIRES: true
 * ENSURES: max (x, y) ==> the greater of x or y
 *
 * Examples:
 *  max (1, 4) ==> 4
 *  max (~4, 0) ==> 0
 *  max (2, 2) ==> 2
 *)
fun max (n1 : int, n2 : int) : int =
    case Int.compare(n1,n2)
     of LESS => n2
      | _ => n1

val 4 = max (1, 4)
val 0 = max (~4, 0)
val 2 = max (2, 2)

(* split : int list -> int list * int * int list
 * REQUIRES: l is non-empty
 * ENSURES: there exist l1,x,l2 such that
 *    split l == (l1,x,l2) and
 *    l == l1 @ x::l2 and
 *    length(l1) and length(l2) differ by no more than 1
 *)
fun split ([] : int list) : (int list * int * int list) = 
    raise Fail "split should never be called on an empty list"
  | split l = 
    let
      val midlen = (length l) div 2
      val front = (List.take (l,midlen))

      (* because we round down, if the list is non-empty, this
       * has at least one thing in it
       *)
      val x :: back = (List.drop (l,midlen))
    in
      (front, x, back)
    end

(* ---------------------------------------------------------------------- *)
(* Functions you, the student, need to implement. *)

(***** Section 2: Depth  *****)

datatype tree =
    Empty
  | Node of (tree * int * tree)

(* Task 2.1 *)
(* tree : tree -> int *)
(* REQUIRES: true *)
(* ENSURES: depth(t) evaluates to the depth of t *)
fun depth (Empty : tree) : int = 0
  | depth (Node (l, x, r) : tree) : int = 1 + max(depth(l), depth(r))

(* ---------------------------------------------------------------------- *)

(***** Section 3: Lists to Trees *****)

(* Task 3.1 *)
(* lsitToTree : int list -> tree *)
(* REQUIRES: true *)
(* ENSURES: listToTree(l) transforms l to a balanced tree *)
fun listToTree ([] : int list) : tree = Empty
  | listToTree (l : int list) : tree = 
    let val (l1, x, l2) = split(l)
    in Node(listToTree(l1), x, listToTree(l2))
    end

(* ---------------------------------------------------------------------- *)

(***** Section 4: Reverse *****)

(* treeToList : tree -> int list
 * REQUIRES: true
 * ENSURES: returns a list of the elements in the tree,
 *           ordered by an in-order traversal 
 *)
fun treeToList (Empty : tree) : int list = []
  | treeToList (Node(l,x,r)) = treeToList l @ (x :: (treeToList r))

(* Task 4.1 *)
(* revT : tree -> tree *)
(* REQUIRES true *)
(* ENSURES revT(t) reverses the tree t *)
fun revT (Empty : tree) : tree = Empty
  | revT (Node(l, x, r) : tree) : tree = Node(revT(r), x, revT(l))

(* ---------------------------------------------------------------------- *)

(***** Section 5: Binary Search *****)

(* Task 5.1 *)

fun binarySearch (Empty : tree, x : int) : bool = false
  | binarySearch (Node(l, y, r) : tree , x : int) = 
    case Int.compare(y, x) of
        GREATER => binarySearch (l, x)
      | EQUAL => true
      | LESS => binarySearch(r, x)
