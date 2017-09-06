structure FindExceptions =
struct

  exception NotFound

  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  (* find : ('a -> bool) -> 'a tree -> 'a
   * requires: true
   * ensures: find p t returns the first value satisfying the predicate
   *       found during an in-order traversal of the tree.
   *)
  fun find (p : 'a -> bool) (Empty : 'a tree) = raise NotFound
    | find (p) (Node(l, x, r)) = if (p x)
                                 then x
                                 else ((find p l)
                                       handle NotFound => (find p r))
end

structure TestFindExceptions =
struct
  open FindExceptions

  val emp : int tree = Empty
  val oneFive : int tree = Node(Empty, 5, Empty)
  val oneSix : int tree = Node(Empty, 6, Empty)
  val oneSeven : int tree = Node(Empty, 7, Empty)

  val oneOdd : int tree = Node(Node(Empty, 2, Empty),
                               6,
                               Node(Empty, 8, Node(Empty, 5, Empty)))

  val ~1 = find (fn x => x=6) oneFive handle NotFound => ~1
  val 6 = find (fn x => x=6) oneSix handle NotFound => ~1
  val 5 = find (fn x => x mod 2 = 1) oneOdd

end

