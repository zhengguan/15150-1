(* ---------------------------------------------------------------------- *)
(* Functions provided by the course staff. *)

(* REQUIRES: true
 * ENSURES: max (x, y) ==> the greater of x or y
 *)
fun max (n1 : int, n2 : int) : int =
    case n1 < n2 of
      true => n2
    | false => n1
               
val 4 = max (1, 4)
val 0 = max (~4, 0)
val 2 = max (2, 2)

(*
 * REQUIRES: l is non-empty
 * ENSURES: there exist l1,x,l2 such that
 *     split l ==> (l1,x,l2) and
 *     l is l1 @ x::l2 and
 *     length(l1) and length(l2) differ by no more than 1
 *)
fun split (l : int list) : (int list * int * int list) =
    case l of
      [] => raise Fail "split should never be called on an empty list"
    | _ => 
      let
        val midlen = (length l) div 2
        val front = (List.take (l,midlen))
        (* because we round down, if the list is non-empty,
         *  this has at least one thing in it *)
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

(* REQUIRES: true
 * ENSURES: Computes the depth of the tree. Empty trees are of depth 0;
 *   singletons are of depth 1.
 *)
fun depth (Empty : tree) : int = 0
  | depth (Node (l, _ , r)) = 1 + max (depth l, depth r)

val 0 = depth Empty
val 3 = depth (Node(Node(Node(Empty,5,Empty),2,Empty),1,Empty))


(* ---------------------------------------------------------------------- *)

(***** Section 3: Lists to Trees *****)

(* Task 3.1 *)
(* REQUIRES: true
 * ENSURES:  transforms an int list into a balanced tree
 *)
fun listToTree ([] : int list) : tree = Empty
  | listToTree (l) = 
    let val (l1, x,  l2) = split l
    in
      Node (listToTree l1, x , listToTree l2)
    end
      
val Empty = listToTree nil
val Node (Empty , 3 , Empty) = listToTree [3]
val Node(Node(Empty,5,Empty),8,Node(Empty,2,Empty)) = listToTree [5,8,2]



(* ---------------------------------------------------------------------- *)

(***** Section 4: Reverse *****)

(* Purpose: compute the list resulting from an in-order traversal of t *)
fun treeToList (t : tree) : int list =
    case t of
      Empty => []
    | Node (l,x,r) => treeToList l @ (x :: (treeToList r))

(* Task 4.1 *)

(* REQUIRES: true
 * ENSURES: Constructs the "mirror image" of the argument tree
 *)
fun revT (Empty : tree) : tree = Empty
  | revT (Node(t1,x,t2)) = Node (revT t2, x , revT t1)
                           
val t1 = (Node(Node(Empty,15,Empty),8,Node(Empty,4,Empty)))
val [4,8,15] = treeToList (revT t1)
val [4,8,15] = rev (treeToList t1)
val Empty = revT Empty
val Node(Empty,1,Empty) = revT (Node(Empty,1,Empty))
val Node(Node(Empty,4,Empty),8,Node(Empty,15,Empty)) = revT t1

(* ---------------------------------------------------------------------- *)

(***** Section 4: Binary Search *****)

(* Task 5.1 *)

(* REQUIRES: t is sorted
 * ENSURES: Assuming t is sorted, determine whether x is in t.
 *
 *    Work and span should be O(depth of t)
 *)
fun binarySearch (Empty : tree, _ : int) : bool = false
  | binarySearch (Node(l,y,r), x) = 
    (case Int.compare (x, y) of
       EQUAL => true
     | LESS => binarySearch (l, x)
     | GREATER => binarySearch (r, x))
    
val t2 = Node (Node (Empty, 5, Empty), 6, Empty)
val t3 = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))

val true = binarySearch (t3, 3)
val false = binarySearch (t2, 3)
val false = binarySearch (Node (t3, 4, t2), 7)
val true = binarySearch (Node (t3, 4, t2), 6)

