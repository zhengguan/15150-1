structure TreeSets : INTSET =
struct
  exception NotYetImplemented
  exception IntentionallyUnimplemented

  exception NotInSet

  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  type set = int tree

  val empty = fn () => Empty

  fun find (n : int) (Empty : set) = false
    | find (n : int) (Node(l,x,r) : set) = 
        if (n = x)
          then true
          else (find n l) orelse (find n r)

  fun insert (n : int) (s : set) = Node(s, n, Empty)

  (* delete : int -> set -> set
   * requires: true
   * ensures: raises IntentiallyUnimplemented
   *
   * You don't have to implement delete; though once you learn mutually recursive
   *   functions, you'll be able to.
   *
   * Luckily, you can write union, intersection, and difference without delete!
   *)
  fun delete (n : int) (s : set) = raise IntentionallyUnimplemented

  fun union (Empty : set) (s2 : set) = s2
    | union (s1 : set) (Empty : set) = s1
    | union (Node(l,x,r) : set) (s2 : set) = Node(l,x,(union r s2))

  fun intersection (Empty : set) (s2 : set) = Empty
    | intersection (s1 : set) (Empty : set) = Empty
    | intersection (Node(l,x,r) : set) (s2 : set) = 
        if (find x s2)
          then Node((intersection l s2),x,(intersection r s2))
          else union (intersection l s2) (intersection r s2)

  fun difference (Empty : set) (s2 : set) = Empty
    | difference (s1 : set) (Empty : set) = Empty
    | difference (Node(l,x,r) : set) (s2 : set) = 
        if (find x s2)
          then union (difference l s2) (difference r s2)
          else Node((difference l s2),x,(difference r s2))

end

structure TestTreeSets =
struct
  (* 'open' brings everything in the TreeSet namespace into this structure.
   *   We can now use 'insert' instead of 'TreeSets.insert'
   *)
  open TreeSets

  (* t = <5, 6> *)
  val t = empty()
  val t = insert 5 t
  val true = find 5 t

  (* Your tests here! *)

end
