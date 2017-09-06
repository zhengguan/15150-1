structure FindExceptions =
struct

  exception NotYetImplemented
  exception NotFound

  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  (* find : ('a -> bool) -> 'a tree -> 'a
   * requires: true
   * ensures: find p t returns the first value satisfying the predicate
   *       found during an in-order traversal of the tree.
   *)
  fun find (p : 'a -> bool) (t : 'a tree) = raise NotYetImplemented

end

structure TestFindExceptions =
struct
  open FindExceptions

  val emp : int tree = Empty

  (* Your test cases go here! *)

end

