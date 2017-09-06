structure RbTree =
struct

exception Unimplemented

(* A possible implementation of RBTs for keys as integers *)
type key = int
val compare : key * key -> order = Int.compare
datatype color = Red | Black
datatype 'v rbtree = Empty
  | Node of 'v rbtree * (color * (key * 'v)) * 'v rbtree

(* Task 3.1 *)
fun isRBT (Empty : 'v rbtree) : bool = true
  | isRBT (Node(l,(c,(k,v)),r) : 'v rbtree) : bool = true

end
